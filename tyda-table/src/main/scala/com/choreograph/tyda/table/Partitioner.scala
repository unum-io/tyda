package com.choreograph.tyda.table
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.ResolverStyle

import scala.deriving.Mirror

import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Date
import com.choreograph.tyda.Expr
import com.choreograph.tyda.FileMetadata
import com.choreograph.tyda.HivePartitionParser
import com.choreograph.tyda.PartitionEncoding
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.shapeless3extras.labelled
import com.choreograph.tyda.shapeless3extras.productInstances
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.toMirroredElemTypes

trait Partitioner {
  def partitions: Seq[String]
  final def path(basePath: String): String = {
    val partitions = this.partitions
    if partitions.isEmpty then basePath
    else basePath + partitions.mkString(if basePath.endsWith("/") then "" else "/", "/", "/")
  }
}

object Partitioner {
  sealed trait ValueCodec[T] extends Serializable {
    def encode(value: T): String
    def decode(value: String): T
  }

  object ValueCodec {
    def apply[T](using value: ValueCodec[T]): ValueCodec[T] = value

    given int: ValueCodec[Int] with {
      def encode(value: Int): String = value.toString
      def decode(value: String): Int = value.toInt
    }
    given str: ValueCodec[String] with {
      def encode(value: String): String = PartitionEncoding.encode(value)
      def decode(value: String): String = PartitionEncoding.decode(value)
    }

    private val isoFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd").withResolverStyle(ResolverStyle.STRICT)

    /** ValueCodec that writes an Int carrying days since epoch as a date in
      * YYYY-MM-dd format.
      */
    def date: ValueCodec[Int] =
      new ValueCodec {
        def encode(value: Int): String = LocalDate.ofEpochDay(value).format(isoFormat)
        def decode(value: String): Int = LocalDate.parse(value, isoFormat).toEpochDay().toInt
      }

    given ValueCodec[Date] =
      new ValueCodec[Date] {
        def encode(value: Date): String = value.toIsoString
        def decode(value: String): Date =
          Date
            .fromIsoString(value)
            .getOrElse(throw new RuntimeException(s"Unable to decode $value as a Date"))
      }

    /** ValueCodec for enums that encodes the enum as a string.
      *
      * Requires that enum have opted into be encoded as a String by deriving a
      * Codec.EnumAsString instance.
      */
    given enumAsString[T: Codec.EnumAsString as codec]: ValueCodec[T] with {
      private val lookupEncode = codec.values.map(v => v -> str.encode(v.toString)).toMap
      private val lookupDecode = lookupEncode.map(_.swap)
      def encode(value: T): String = lookupEncode(value)
      def decode(value: String): T = lookupDecode(value)
    }
  }

  trait Creator[V, P <: Partitioner] {
    def create(value: V): P
    def unfixed: P
  }

  object Creator {
    type From[V] = [P <: Partitioner] =>> Creator[V, P]
  }

  /** Type class with functionality for reading from a Partitioner.
    */
  trait Determinator[V, P <: Partitioner] extends Serializable {

    /** Given a path, decode the partition values from it. */

    def decode(path: String): V

    /** Given a partitioner `p`, produce a predicate that can be used to filter
      * a dataset to only rows matching the partitioner.
      */
    def predicate(p: P): Expr[V] => Expr[Boolean]
  }

  object Determinator {
    type Into[V] = [P <: Partitioner] =>> Determinator[V, P]
  }

  type None = None.type
  case object None extends Partitioner {
    def partitions: Seq[String] = Seq.empty

    extension [M: Codec](source: Source[M, None]) {
      def read: Dataset[M] = source.asDataset(None).values
      def readWithFileMetadata: Dataset[(FileMetadata, M)] = source.asDataset(None).withFileMetadata
    }

    given Determinator[EmptyTuple, None] with {
      def decode(path: String): EmptyTuple = EmptyTuple
      def predicate(p: None): Expr[EmptyTuple] => Expr[Boolean] = _ => lit(true)
    }

    given Creator[EmptyTuple, None] with {
      def create(value: EmptyTuple): None = None
      def unfixed: None = None
    }
  }

  sealed trait Hive[T] extends Partitioner

  object Hive {
    extension [M: Codec](source: Source[M, Partitioner.Hive[EmptyTuple]]) {
      def read: Dataset[M] = source.asDataset(Partitioner.Hive.fromValue(EmptyTuple)).values
    }

    private final case class HiveImpl[T: K0.ProductInstancesOf[
      ValueCodec
    ] as codecs: Labelling, Elems <: Tuple](values: Tuple.Map[Elems, Seq])(using
        Mirror.ProductOf[T] { type MirroredElemTypes = Elems }
    ) extends Hive[T] {
      def partitions: Seq[String] =
        codecs
          .labelled
          .zip(productInstances(values))
          .foldLeft0(Vector.empty[String]) { [t] => (acc, triple) =>
            val ((label, codec), values) = triple
            val encoded = Partitioner.toGlobString(values.map(codec.encode(_)))
            acc :+ s"${label}=${encoded}"
          }
    }

    def unfixed[T: K0.ProductInstancesOf[ValueCodec] as codecs: Labelling: Mirror.ProductOf]: Hive[T] =
      fromSeqs(codecs.mapK[Seq]([t] => _ => Seq.empty).toTuple)

    def fromSeqs[T: K0.ProductInstancesOf[ValueCodec]: Labelling: Mirror.ProductOf as m](
        values: Tuple.Map[m.MirroredElemTypes, Seq]
    ): Hive[T] = HiveImpl(values)

    def fromValue[T: K0.ProductInstancesOf[ValueCodec]: Labelling: Mirror.ProductOf](value: T): Hive[T] =
      Hive.fromSeqs(value.toMirroredElemTypes.map([t] => Seq(_)))

    given [T: Mirror.ProductOf as m: K0.ProductInstancesOf[ValueCodec] as instances: Labelling]
        => Determinator[T, Hive[T]] {
      val decoder = HivePartitionParser.make(instances.mapK([t] => codec => codec.decode(_)))
      def decode(path: String): T =
        decoder(path) match {
          case Right(value) => value
          case Left(error) => throw new RuntimeException(error)
        }

      def predicate(p: Hive[T]): Expr[T] => Expr[Boolean] = { e =>
        val exprs = tupleInstances(Expr.unapply(e))
        val seqValues = tupleInstances(p match {
          // TYPE SAFETY: MirroredElemTypes is uniquely determined by T
          case p: HiveImpl[T, m.MirroredElemTypes @unchecked] => p.values
        })
        exprs
          .zip(seqValues)
          .foldLeft0(lit(true)) { [t] => (acc, pair) =>
            val (expr, values) = pair
            acc && values.map(v => expr == v).reduceOption(_ || _).getOrElse(lit(true))
          }
      }
    }

    given [T: K0.ProductInstancesOf[ValueCodec]: Mirror.ProductOf as m: Labelling]: Creator[T, Hive[T]] with {
      def create(value: T): Hive[T] = Hive.fromValue(value)
      def unfixed: Hive[T] = Hive.unfixed
    }
  }

  def toGlobString(values: Seq[String]): String =
    values match {
      case Seq() => "*"
      case Seq(v) => v
      case many => many.mkString("{", ",", "}")
    }

}
