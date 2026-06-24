package com.choreograph.tyda.spark

import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.<=
import scala.compiletime.ops.int.>=

import org.apache.spark.sql.types.ArrayType
import org.apache.spark.sql.types.BinaryType
import org.apache.spark.sql.types.BooleanType
import org.apache.spark.sql.types.ByteType
import org.apache.spark.sql.types.DataType
import org.apache.spark.sql.types.DateType
import org.apache.spark.sql.types.DecimalType
import org.apache.spark.sql.types.DoubleType
import org.apache.spark.sql.types.FloatType
import org.apache.spark.sql.types.IntegerType
import org.apache.spark.sql.types.LongType
import org.apache.spark.sql.types.MapType
import org.apache.spark.sql.types.ShortType
import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.types.TimestampType

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Field
import com.choreograph.tyda.rewrite.ApproximatedCodec

object StructTypeToCodec {

  /** Build a Codec mirroring a Spark schema. A nullable column / element /
    * map-value becomes `Codec.Option`, so it lines up with how optionality is
    * modelled everywhere else.
    *
    * Fields whose Spark type has no corresponding tyda Codec are skipped. This
    * propagates through containers: an array/map whose element/value type is
    * unsupported is itself skipped.
    */
  def apply(schema: StructType): ApproximatedCodec[?] = structCodec(schema)

  private def structCodec(struct: StructType): ApproximatedCodec[?] = {
    val nameAndApproximatedCodec = struct
      .fields
      .flatMap(f => codecOf(f.dataType, f.nullable).map(f.name -> _))
      .toSeq
    val tydaFields = nameAndApproximatedCodec.map((name, approx) => Field(name, approx.codec))
    val isExact = struct.size == tydaFields.size && nameAndApproximatedCodec.forall(_._2.isExact)
    ApproximatedCodec(Codec.unsafeNamedTuple(tydaFields), isExact)
  }

  private def codecOf(dataType: DataType, nullable: Boolean): Option[ApproximatedCodec[?]] =
    val inner = codecOf(dataType)
    if !nullable then inner else inner.map(approx => approx.copy(codec = Codec.Option(approx.codec)))

  private def codecOf(dataType: DataType): Option[ApproximatedCodec[?]] =
    def exact[T](codec: Codec[T]) = Some(ApproximatedCodec(codec, true))
    dataType match {
      case ByteType => exact(Codec.Byte)
      case ShortType => exact(Codec.Short)
      case IntegerType => exact(Codec.Int)
      case LongType => exact(Codec.Long)
      case FloatType => exact(Codec.Float)
      case DoubleType => exact(Codec.Double)
      case BooleanType => exact(Codec.Boolean)
      case _: StringType => exact(Codec.String)
      case BinaryType => exact(Codec.Bytes)
      case DateType => exact(Codec.Date)
      case TimestampType => exact(Codec.TimestampMicros)
      case d: DecimalType => exact(decimalCodec(d.precision, d.scale))
      case a: ArrayType =>
        codecOf(a.elementType, a.containsNull).map(approx => approx.copy(Codec.Seq(approx.codec)))
      case m: MapType => for {
          key <- codecOf(m.keyType)
          value <- codecOf(m.valueType, m.valueContainsNull)
        } yield ApproximatedCodec(Codec.Map(key.codec, value.codec), key.isExact && value.isExact)
      case s: StructType => Some(structCodec(s))
      case _ => None
    }

  private def decimalCodec[P <: Int, S <: Int](p: P, s: S): Codec[Decimal[P, S]] = {
    assert(s <= p && s >= 0 && p >= 1 && p <= Decimal.MaxPrecision)
    // TYPE SAFETY: Spark only supports 128 bit decimal types
    given ((S <= P && S >= 0 && P >= 1 && P <= Decimal.MaxPrecision) =:= true) =
      summon[true =:= true].asInstanceOf
    given ValueOf[P] = ValueOf(p)
    given ValueOf[S] = ValueOf(s)
    Codec.Decimal[P, S](p, s)
  }
}
