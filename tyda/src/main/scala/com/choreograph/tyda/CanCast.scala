package com.choreograph.tyda

import scala.annotation.implicitNotFound
import scala.compiletime.ops.int.-
import scala.compiletime.ops.int.>=

@implicitNotFound(
  "Cannot cast from ${From} to ${To}. If it a valid cast that might fail then use tryCast instead."
)
sealed trait CanCast[From, To: Codec] {
  def codec: Codec[To] = summon[Codec[To]]
}

object CanCast {
  type From[From] = [To] =>> CanCast[From, To]
  private[tyda] case object ByteToShort extends CanCast[Byte, Short]
  private[tyda] case object ByteToInt extends CanCast[Byte, Int]
  private[tyda] case object ByteToLong extends CanCast[Byte, Long]
  private[tyda] case object ByteToFloat extends CanCast[Byte, Float]
  private[tyda] case object ByteToDouble extends CanCast[Byte, Double]

  private[tyda] case object ShortToInt extends CanCast[Short, Int]
  private[tyda] case object ShortToLong extends CanCast[Short, Long]
  private[tyda] case object ShortToFloat extends CanCast[Short, Float]
  private[tyda] case object ShortToDouble extends CanCast[Short, Double]

  private[tyda] case object IntToLong extends CanCast[Int, Long]
  private[tyda] case object IntToFloat extends CanCast[Int, Float]
  private[tyda] case object IntToDouble extends CanCast[Int, Double]

  private[tyda] case object LongToFloat extends CanCast[Long, Float]
  private[tyda] case object LongToDouble extends CanCast[Long, Double]

  private[tyda] case object FloatToDouble extends CanCast[Float, Double]
  private[tyda] case object DoubleToFloat extends CanCast[Double, Float]

  private[tyda] case object ByteToString extends CanCast[Byte, String]
  private[tyda] case object ShortToString extends CanCast[Short, String]
  private[tyda] case object IntToString extends CanCast[Int, String]
  private[tyda] case object LongToString extends CanCast[Long, String]

  private[tyda] case object StringToBytes extends CanCast[String, Binary]

  private[tyda] final case class DecimalToFloat[P <: Int, S <: Int]() extends CanCast[Decimal[P, S], Float]
  private[tyda] final case class DecimalToDouble[P <: Int, S <: Int]() extends CanCast[Decimal[P, S], Double]

  private[tyda] final case class DecimalToDecimal[P1 <: Int, S1 <: Int, P2 <: Int, S2 <: Int]()(using
      val valid: Decimal.Valid[P2, S2],
      val maxIncrease: P2 - S2 >= P1 - S1 =:= true,
      val scaleIncrease: S2 >= S1 =:= true
  ) extends CanCast[Decimal[P1, S1], Decimal[P2, S2]]

  private[tyda] final case class ByteToDecimal[P <: Int, S <: Int]()(using
      val valid: Decimal.Valid[P, S],
      val ev: P - S >= 3 =:= true
  ) extends CanCast[Byte, Decimal[P, S]]

  private[tyda] final case class ShortToDecimal[P <: Int, S <: Int]()(using
      val valid: Decimal.Valid[P, S],
      val ev: P - S >= 5 =:= true
  ) extends CanCast[Short, Decimal[P, S]]

  private[tyda] final case class IntToDecimal[P <: Int, S <: Int]()(using
      val valid: Decimal.Valid[P, S],
      val ev: P - S >= 10 =:= true
  ) extends CanCast[Int, Decimal[P, S]]

  private[tyda] final case class LongToDecimal[P <: Int, S <: Int]()(using
      val valid: Decimal.Valid[P, S],
      val ev: P - S >= 19 =:= true
  ) extends CanCast[Long, Decimal[P, S]]

  private[tyda] final case class SeqToSeq[From, To: Codec](canCast: CanCast[From, To])
      extends CanCast[Seq[From], Seq[To]]

  given byteToShort: CanCast[Byte, Short] = ByteToShort
  given byteToInt: CanCast[Byte, Int] = ByteToInt
  given byteToLong: CanCast[Byte, Long] = ByteToLong
  given byteToFloat: CanCast[Byte, Float] = ByteToFloat
  given byteToDouble: CanCast[Byte, Double] = ByteToDouble

  given shortToInt: CanCast[Short, Int] = ShortToInt
  given shortToLong: CanCast[Short, Long] = ShortToLong
  given shortToFloat: CanCast[Short, Float] = ShortToFloat
  given shortToDouble: CanCast[Short, Double] = ShortToDouble

  given intToLong: CanCast[Int, Long] = IntToLong
  given intToFloat: CanCast[Int, Float] = IntToFloat
  given intToDouble: CanCast[Int, Double] = IntToDouble

  given longToFloat: CanCast[Long, Float] = LongToFloat
  given longToDouble: CanCast[Long, Double] = LongToDouble

  given floatToDouble: CanCast[Float, Double] = FloatToDouble
  given doubleToFloat: CanCast[Double, Float] = DoubleToFloat

  given byteToString: CanCast[Byte, String] = ByteToString
  given shortToString: CanCast[Short, String] = ShortToString
  given intToString: CanCast[Int, String] = IntToString
  given longToString: CanCast[Long, String] = LongToString

  given stringToBytes: CanCast[String, Binary] = StringToBytes

  given decimalToFloat[P <: Int, S <: Int]: CanCast[Decimal[P, S], Float] = DecimalToFloat()
  given decimalToDouble[P <: Int, S <: Int]: CanCast[Decimal[P, S], Double] = DecimalToDouble()

  given decimalToDecimal[P1 <: Int, S1 <: Int, P2 <: Int, S2 <: Int](using
      valid: Decimal.Valid[P2, S2],
      maxIncrease: P2 - S2 >= P1 - S1 =:= true,
      scaleIncrease: S2 >= S1 =:= true
  ): CanCast[Decimal[P1, S1], Decimal[P2, S2]] = DecimalToDecimal()

  given byteToDecimal[P <: Int, S <: Int](using
      valid: Decimal.Valid[P, S],
      ev: P - S >= 3 =:= true
  ): CanCast[Byte, Decimal[P, S]] = ByteToDecimal()

  given shortToDecimal[P <: Int, S <: Int](using
      valid: Decimal.Valid[P, S],
      ev: P - S >= 5 =:= true
  ): CanCast[Short, Decimal[P, S]] = ShortToDecimal()

  given intToDecimal[P <: Int, S <: Int](using
      valid: Decimal.Valid[P, S],
      ev: P - S >= 10 =:= true
  ): CanCast[Int, Decimal[P, S]] = IntToDecimal()

  given longToDecimal[P <: Int, S <: Int](using
      valid: Decimal.Valid[P, S],
      ev: P - S >= 19 =:= true
  ): CanCast[Long, Decimal[P, S]] = LongToDecimal()

  given seqToSeq[From, To](using canCast: CanCast[From, To]): CanCast[Seq[From], Seq[To]] = {
    given Codec[To] = canCast.codec
    SeqToSeq(canCast)
  }
}
