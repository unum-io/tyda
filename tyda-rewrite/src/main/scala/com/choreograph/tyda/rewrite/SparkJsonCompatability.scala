package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CanTryCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Format
import com.choreograph.tyda.NumericsReadMode.FailableReadAdapter
import com.choreograph.tyda.NumericsReadMode.ReadAdapter
import com.choreograph.tyda.NumericsReadMode.buildFailableReadAdapter
import com.choreograph.tyda.NumericsReadMode.buildReadAdapter
import com.choreograph.tyda.SimpleTypeName
import com.choreograph.tyda.functions.microsToDuration
import com.choreograph.tyda.rewrite.Cast.buildConverterDown

object SparkJsonCompatability {
  object AdaptToJson extends ExprRule {
    def apply[T](expr: ExprNode[T]): ExprNode[T] =
      expr match {
        case AdaptToJson(converted) => converted
        case _ => expr
      }

    def unapply[T](expr: ExprNode[T]): Option[ExprNode[T]] =
      expr match {
        case ExprNode.ToJson(inner) => writeConverter(inner.codec).map(_(inner)).map(ExprNode.ToJson(_))
        case _ => None
      }
  }

  object ConvertFromJson extends ExprRule {
    def apply[T](expr: ExprNode[T]): ExprNode[T] =
      expr match {
        case ConvertFromJson(converted) => converted
        case _ => expr
      }

    def unapply[T](expr: ExprNode[T]): Option[ExprNode[T]] =
      expr match {
        case ExprNode.FromJson(inner, codec) =>
          fromJsonAdapter(codec).map { case FailableReadAdapter(readCodec, cast, maybeCheck) =>
            val converted = ExprNode.FromJson(inner, readCodec)
            val checked = maybeCheck match {
              case Some(check) => converted.filter(e => check(Expr.lift(e)).node)
              case None => converted
            }
            checked.map(e => cast(Expr.lift(e)).node)
          }
        case _ => None
      }
  }

  object AdaptReads extends DatasetRule {
    def apply[T](ds: Dataset[T]): Dataset[T] =
      ds match {
        case AdaptReads(adapted) => adapted
        case _ => ds
      }

    def unapply[T](ds: Dataset[T]): Option[Dataset[T]] =
      ds match {
        case Dataset.ReadPath(path, Format.Json, unpivot, filenameGlobFilter, codec) => readAdapter(codec)
            .map { case ReadAdapter(codec, cast) =>
              Dataset.ReadPath(path, Format.Json, unpivot, filenameGlobFilter, codec).select(cast)
            }
        case Dataset.ReadPathWithHivePartitions(
              basePath,
              path,
              Format.Json,
              filenameGlobFilter,
              pCodec,
              mCodec
            ) => readAdapter(mCodec).map { case ReadAdapter(readCodec, cast) =>
            val onlyHereToHelpWithTypeInference = Dataset
              .ReadPathWithHivePartitions(basePath, path, Format.Json, filenameGlobFilter, pCodec, readCodec)
              .select(_._1, cast.compose(_._2))
            onlyHereToHelpWithTypeInference
          }
        case _ => None
      }
  }

  object ConvertWrites extends ActionRule {
    def unapply(action: Dataset.Action): Option[Dataset.Action] =
      action match {
        case Dataset.Action.Write(input, path, Format.Json) => writeConverter(input.codec).map { cast =>
            Dataset.Action.Write(input.select(cast.cast), path, Format.Json)
          }
        case _ => None
      }

    def apply(action: Dataset.Action): Dataset.Action =
      action match {
        case ConvertWrites(converted) => converted
        case _ => action
      }
  }

  /* Spark reads json as if all fields are nullable:
   * https://github.com/apache/spark/blob/50514c5271e0fae3f2546c4edea9da8ee3323344/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/expressions/jsonExpressions.scala#L268-L271
   * So to prevent unpected nulls in the result we also change the codec to be all nullable and then check it
   * before changing it back to the original codec. */
  private def fromJsonAdapter[T](codec: Codec[T]): Option[FailableReadAdapter[T, ?]] =
    failableReadAdapter[T](codec) match {
      case Some(adapter) =>
        AsAllNullable.children(adapter.codec).map(_.compose(adapter)).orElse(Some(adapter))
      case None => AsAllNullable.children(codec)
    }

  private def failableReadAdapter[T](codec: Codec[T]): Option[FailableReadAdapter[T, ?]] =
    buildFailableReadAdapter(codec)([t] =>
      _ match {
        case Codec.Byte => checkCast[Byte]
        case Codec.Short => checkCast[Short]
        case Codec.Int => checkCast[Int]
        case Codec.Long => checkCast[Long]
        case other => readAdapterRule(other).map(_.asFailable)
      }
    )

  private val readAdapterRule: [t] => Codec[t] => Option[ReadAdapter[t, ?]] = [t] =>
    _ match {
      case Codec.Byte => tryCastAndExpect[Byte]
      case Codec.Short => tryCastAndExpect[Short]
      case Codec.Int => tryCastAndExpect[Int]
      case Codec.Long => tryCastAndExpect[Long]
      case Codec.Map(given Codec[k], given Codec[v]) => Some(ReadAdapter(
          Codec[Seq[(key: k, value: v)]],
          // TODO: Would be nice if this could just be a cast instead of a higher order function
          seq => seq.map { case Expr(key, value) => (key, value) }.toMap
        ))
      case Codec.DurationMicros => Some(ReadAdapter(Codec[Long], microsToDuration))
      case _ => None
    }

  private def readAdapter[T](codec: Codec[T]): Option[ReadAdapter[T, ?]] =
    buildReadAdapter(codec)(readAdapterRule)

  private def checkCast[To: SimpleTypeName](using
      CanTryCast[String, To]
  ): Option[FailableReadAdapter[To, ?]] =
    Some(FailableReadAdapter(Codec[String], _.tryCast[To].get, Some(!_.tryCast[To].isEmpty)))

  private def tryCastAndExpect[To: SimpleTypeName](using CanTryCast[String, To]): Option[ReadAdapter[To, ?]] =
    Some(ReadAdapter(
      Codec[String],
      _.tryCast[To].expect(s"Read value was out of bounds or invalid for ${SimpleTypeName.name[To]}")
    ))

  private def writeConverter[T](codec: Codec[T]): Option[Cast[T, ?]] = {
    import com.choreograph.tyda.Expr.toMicros
    buildConverterDown(codec)([t] =>
      _ match {
        case Codec.Map(_: Codec[k], _: Codec[v]) => Some(Cast((expr: Expr[Map[k, v]]) => expr.entries))
        case Codec.DurationMicros => Some(Cast(_.toMicros))
        case _ => None
      }
    )
  }
}
