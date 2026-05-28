package com.choreograph.tyda.rewrite
import com.choreograph.tyda.CanTryCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Format
import com.choreograph.tyda.NumericsReadMode.ReadAdapter
import com.choreograph.tyda.NumericsReadMode.buildReadAdapter
import com.choreograph.tyda.SimpleTypeName
import com.choreograph.tyda.functions.microsToDuration
import com.choreograph.tyda.rewrite.Cast.buildConverterDown

object SparkJsonCompatability {

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

  private def readAdapter[T](codec: Codec[T]): Option[ReadAdapter[T, ?]] =
    buildReadAdapter(codec)([t] =>
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
    )

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
