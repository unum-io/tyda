package com.choreograph.tyda.bigquery

import java.nio.file.Files
import java.nio.file.Path

import scala.sys.process.Process
import scala.sys.process.ProcessLogger

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArrayReentrant
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.github.plokhotnyuk.jsoniter_scala.core.writeToArrayReentrant
import org.scalatest.Assertions.assume
import org.scalatest.Assertions.pending

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Field
import com.choreograph.tyda.Runner
import com.choreograph.tyda.json.CodecToJsoniter
import com.choreograph.tyda.rewrite.CollectionOrNullableCollectionCodec
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.sql.DatasetToSqlError
import com.choreograph.tyda.sql.RenderedMultiStatement
import com.choreograph.tyda.sql.SqlDialect
import com.choreograph.tyda.sql.toSql
import com.choreograph.tyda.testsuites.BigQueryIntegrationTestEnvVariables
import com.choreograph.tyda.unreachable

object GoogleSqlTestRunner {
  private val ExecutableEnv = "TYDA_GOOGLESQL_EXECUTE_QUERY"

  def configuredOrSkip: GoogleSqlTestRunner = {
    BigQueryIntegrationTestEnvVariables.skipIfProjectIsSet()
    sys.env.get(ExecutableEnv).map(Path.of(_)) match {
      case Some(executable) if Files.isExecutable(executable) => new GoogleSqlTestRunner(executable)
      case _ =>
        assume(
          condition = false,
          s"Set $ExecutableEnv to the executable provided by dev/download-googlesql.sh to run local GoogleSQL tests"
        )
        unreachable("Test skipped by assume")
    }
  }

  private[bigquery] def errorMessageFromBoxOutput(output: String, query: String, stderr: String): String =
    output
      .linesIterator
      .collectFirst { case line if line.startsWith("ERROR: ") => line.stripPrefix("ERROR: ") }
      .getOrElse(
        s"GoogleSQL returned no JSON output and no recognized error. SQL:\n$query\nStandard output:\n$output\nStandard error:\n$stderr"
      )

  private[bigquery] def escapeMacrosInStringLiterals(sql: String): String = {
    val result = StringBuilder()
    var inString = false
    var index = 0
    while index < sql.length do
      sql(index) match {
        case '\'' =>
          inString = !inString
          result.append('\'')
          index += 1
        case '\\' if inString && index + 1 < sql.length =>
          result.append(sql(index)).append(sql(index + 1)): Unit
          index += 2
        case '$' if inString =>
          result.append("$' '")
          index += 1
        case character =>
          result.append(character)
          index += 1
      }
    result.result()
  }
}

final class GoogleSqlTestRunner private (executable: Path) extends Runner {
  def sql(ds: Dataset[?] | Dataset.Action): RenderedMultiStatement =
    toSql(ds, SqlDialect.GoogleSql) match {
      case Left(DatasetToSqlError.RequiresUdfCapability(_)) =>
        pending
        unreachable("Test should be skipped by pending")
      case Left(DatasetToSqlError.NotImplemented(msg)) =>
        pending
        unreachable(s"Unimplemented feature: $msg")
      case Right(plan) => plan
    }

  def collect[T](ds: Dataset[T]): Seq[T] = {
    val rendered = sql(ds).single
    val query = GoogleSqlTestRunner.escapeMacrosInStringLiterals(
      s"SELECT TO_JSON_STRING(result) AS value FROM ($rendered) AS result"
    )
    val encodedRows = run(query)
    val jsonCodec = GoogleSqlJsonCodec.create(using ds.codec)
    encodedRows.map(encoded => readFromString(encoded)(using jsonCodec))
  }

  def execute(ds: Dataset.Action): Unit =
    throw UnsupportedOperationException("GoogleSQL reference execution does not support BigQuery actions")

  def explain[T](ds: Dataset[T]): String = sql(ds).single
  def explain(action: Dataset.Action): String = sql(action).single

  private def run(query: String): Seq[String] = {
    val queryFile = Files.createTempFile("tyda-googlesql-", ".sql")
    try {
      Files.writeString(queryFile, query)
      val (exitCode, stdout, stderr) = runQuery(queryFile, "json")
      if exitCode != 0 then
        throw RuntimeException(
          s"GoogleSQL exited with status $exitCode. SQL:\n$query\nStandard output:\n$stdout\nStandard error:\n$stderr"
        )
      val output = stdout.trim
      if output.isEmpty then {
        val (boxExitCode, boxOutput, boxError) = runQuery(queryFile, "box")
        if boxExitCode != 0 then
          throw RuntimeException(
            s"GoogleSQL exited with status $boxExitCode. SQL:\n$query\nStandard output:\n$boxOutput\nStandard error:\n$boxError"
          )
        throw RuntimeException(GoogleSqlTestRunner.errorMessageFromBoxOutput(boxOutput, query, boxError))
      } else
        try readFromString(output)(using GoogleSqlResultCodec)
        catch {
          case error: Throwable => throw RuntimeException(
              s"Unable to decode GoogleSQL result. SQL:\n$query\nOutput:\n$output",
              error
            )
        }
    } finally Files.deleteIfExists(queryFile): Unit
  }

  private def runQuery(queryFile: Path, outputMode: String): (Int, String, String) = {
    val command =
      Seq(executable.toString, "--catalog=none", s"--output_mode=$outputMode", s"--sql_file=$queryFile")
    val stdout = StringBuilder()
    val stderr = StringBuilder()
    val logger = ProcessLogger(
      line => { stdout.append(line).append('\n'): Unit },
      line => { stderr.append(line).append('\n'): Unit }
    )
    val exitCode = Process(command).!(logger)
    (exitCode, stdout.result(), stderr.result())
  }
}

private[bigquery] object GoogleSqlJsonCodec {
  def create[T: Codec]: JsonValueCodec[T] = {
    val codec = summon[Codec[T]]
    val jsonCodec = CodecToJsoniter.create[T]
    new JsonValueCodec[T] {
      def decodeValue(in: JsonReader, default: T): T = {
        val tydaJson = GoogleSqlJsonNormalizer.normalize(in.readRawValAsBytes(), codec)
        readFromArrayReentrant(tydaJson)(using jsonCodec)
      }

      def encodeValue(value: T, out: JsonWriter): Unit = jsonCodec.encodeValue(value, out)

      def nullValue: T = jsonCodec.nullValue
    }
  }

}

private object GoogleSqlJsonNormalizer {
  private enum Json {
    case Raw(value: scala.Array[Byte])
    case JsonArray(values: Seq[Json])
    case Object(fields: Seq[(String, Json)])
  }

  private object JsonCodec extends JsonValueCodec[Json] {
    def decodeValue(in: JsonReader, default: Json): Json = read(in)

    def encodeValue(value: Json, out: JsonWriter): Unit =
      value match {
        case Json.Raw(value) => out.writeRawVal(value)
        case Json.JsonArray(values) =>
          out.writeArrayStart()
          values.foreach(encodeValue(_, out))
          out.writeArrayEnd()
        case Json.Object(fields) =>
          out.writeObjectStart()
          fields.foreach { case (name, value) =>
            out.writeKey(name)
            encodeValue(value, out)
          }
          out.writeObjectEnd()
      }

    def nullValue: Json = Json.Raw("null".getBytes(java.nio.charset.StandardCharsets.UTF_8))
  }

  def normalize(encoded: Array[Byte], codec: Codec[?]): Array[Byte] = {
    val googleSqlJson = readFromArrayReentrant(encoded)(using JsonCodec)
    val tydaJson = normalizeTopLevel(googleSqlJson, codec)
    writeToArrayReentrant(tydaJson)(using JsonCodec)
  }

  private def normalizeTopLevel(value: Json, codec: Codec[?]): Json =
    codec match {
      case Codec.Product(_, _, _) => normalize(value, codec)
      case Codec.FromInjection(_, inner) => normalizeTopLevel(value, inner)
      case _ => normalizeObject(value, Map("value" -> codec))
    }

  private def normalize(value: Json, codec: Codec[?]): Json =
    codec match {
      case Codec.Option(element @ Codec.Option(_)) => normalizeObject(value, Map("value" -> element))
      case Codec.Option(element) => normalize(value, element)
      case Codec.Seq(element) => normalizeArray(value, element)
      case Codec.Map(keyCodec, valueCodec) => normalizeMap(value, keyCodec, valueCodec)
      case Codec.Product(_, fields, _) =>
        val fieldCodecs = fields
          .mapConst[Field[?]]([t] => identity(_))
          .map(field => field.name -> field.codec)
          .toMap
        normalizeObject(value, fieldCodecs)
      case Codec.FromInjection(_, inner) => normalize(value, inner)
      case _ => value
    }

  private def normalizeArray(value: Json, elementCodec: Codec[?]): Json =
    value match {
      case Json.JsonArray(values) =>
        val wrapped = CollectionOrNullableCollectionCodec.unapply(elementCodec).isDefined
        Json.JsonArray(values.map { value =>
          val element = if wrapped then unwrapCollectionElement(value) else value
          normalize(element, elementCodec)
        })
      case _ => value
    }

  private def normalizeMap(value: Json, keyCodec: Codec[?], valueCodec: Codec[?]): Json =
    value match {
      case Json.JsonArray(values) =>
        Json.JsonArray(values.map(normalizeObject(_, Map("key" -> keyCodec, "value" -> valueCodec))))
      case _ => value
    }

  private def unwrapCollectionElement(value: Json): Json =
    value match {
      case Json.Object(fields) => fields.collectFirst { case ("value", value) => value }.getOrElse(value)
      case _ => value
    }

  private def normalizeObject(value: Json, fieldCodecs: Map[String, Codec[?]]): Json =
    value match {
      case Json.Object(fields) => Json.Object(fields.map { case (name, value) =>
          name -> fieldCodecs.get(name).fold(value)(normalize(value, _))
        })
      case _ => value
    }

  private def read(in: JsonReader): Json =
    if in.isNextToken('{') then readObject(in)
    else {
      in.rollbackToken()
      if in.isNextToken('[') then readArray(in)
      else {
        in.rollbackToken()
        Json.Raw(in.readRawValAsBytes())
      }
    }

  private def readObject(in: JsonReader): Json = {
    val fields = Vector.newBuilder[(String, Json)]
    if !in.isNextToken('}') then {
      in.rollbackToken()
      while {
        fields += in.readKeyAsString() -> read(in)
        in.isNextToken(',')
      } do ()
      if !in.isCurrentToken('}') then in.objectEndOrCommaError()
    }
    Json.Object(fields.result())
  }

  private def readArray(in: JsonReader): Json = {
    val values = Vector.newBuilder[Json]
    if !in.isNextToken(']') then {
      in.rollbackToken()
      while {
        values += read(in)
        in.isNextToken(',')
      } do ()
      if !in.isCurrentToken(']') then in.arrayEndOrCommaError()
    }
    Json.JsonArray(values.result())
  }
}

private[bigquery] object GoogleSqlResultCodec extends JsonValueCodec[Seq[String]] {
  def decodeValue(in: JsonReader, default: Seq[String]): Seq[String] = {
    if !in.isNextToken('{') then in.decodeError("Expected GoogleSQL JSON result object")
    val rows = Vector.newBuilder[String]
    if !in.isNextToken('}') then {
      in.rollbackToken()
      while {
        in.readKeyAsString() match {
          case "row" => readRows(in, rows)
          case _ => in.skip()
        }
        in.isNextToken(',')
      } do ()
      if !in.isCurrentToken('}') then in.objectEndOrCommaError()
    }
    rows.result()
  }

  def encodeValue(x: Seq[String], out: com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter): Unit =
    out.writeArrayStart()
    x.foreach(out.writeVal)
    out.writeArrayEnd()

  def nullValue: Seq[String] = Seq.empty

  private def readRows(
      in: JsonReader,
      rows: scala.collection.mutable.Builder[String, Vector[String]]
  ): Unit = {
    if !in.isNextToken('[') then in.decodeError("Expected GoogleSQL result rows")
    else if !in.isNextToken(']') then {
      in.rollbackToken()
      while {
        rows += readRow(in)
        in.isNextToken(',')
      } do ()
      if !in.isCurrentToken(']') then in.arrayEndOrCommaError()
    }
  }

  private def readRow(in: JsonReader): String = {
    if !in.isNextToken('{') then in.decodeError("Expected GoogleSQL result row")
    var value: Option[String] = None
    if !in.isNextToken('}') then {
      in.rollbackToken()
      while {
        in.readKeyAsString() match {
          case "value" => value = Some(in.readString(null))
          case _ => in.skip()
        }
        in.isNextToken(',')
      } do ()
      if !in.isCurrentToken('}') then in.objectEndOrCommaError()
    }
    value.getOrElse(in.decodeError("GoogleSQL result row has no value column"))
  }
}
