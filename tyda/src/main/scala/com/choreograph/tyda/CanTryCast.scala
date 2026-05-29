package com.choreograph.tyda

sealed trait CanTryCast[From, To: Codec] {
  def codec: Codec[To] = summon[Codec[To]]
}

object CanTryCast {
  type From[From] = [To] =>> CanTryCast[From, To]

  private[tyda] final case class FromCanCast[From, To](canCast: CanCast[From, To])
      extends CanTryCast[From, To](using canCast.codec)

  private[tyda] case object LongToByte extends CanTryCast[Long, Byte]
  private[tyda] case object LongToShort extends CanTryCast[Long, Short]
  private[tyda] case object LongToInt extends CanTryCast[Long, Int]

  private[tyda] case object IntToByte extends CanTryCast[Int, Byte]
  private[tyda] case object IntToShort extends CanTryCast[Int, Short]

  private[tyda] case object ShortToByte extends CanTryCast[Short, Byte]

  private[tyda] final case class FloatToDecimal[P <: Int, S <: Int]()(using val valid: Decimal.Valid[P, S])
      extends CanTryCast[Float, Decimal[P, S]]
  private[tyda] final case class DoubleToDecimal[P <: Int, S <: Int]()(using val valid: Decimal.Valid[P, S])
      extends CanTryCast[Double, Decimal[P, S]]

  private[tyda] final case class DecimalToDecimal[P1 <: Int, S1 <: Int, P2 <: Int, S2 <: Int]()(using
      val valid: Decimal.Valid[P2, S2]
  ) extends CanTryCast[Decimal[P1, S1], Decimal[P2, S2]]

  private[tyda] case object StringToByte extends CanTryCast[String, Byte]
  private[tyda] case object StringToShort extends CanTryCast[String, Short]
  private[tyda] case object StringToInt extends CanTryCast[String, Int]
  private[tyda] case object StringToLong extends CanTryCast[String, Long]

  given canCast: [T, R] => CanCast[T, R] => CanTryCast[T, R] = FromCanCast[T, R](summon)

  given longToByte: CanTryCast[Long, Byte] = LongToByte
  given longToShort: CanTryCast[Long, Short] = LongToShort
  given longToInt: CanTryCast[Long, Int] = LongToInt

  given intToByte: CanTryCast[Int, Byte] = IntToByte
  given intToShort: CanTryCast[Int, Short] = IntToShort

  given shortToByte: CanTryCast[Short, Byte] = ShortToByte

  given floatToDecimal[P <: Int, S <: Int](using
      valid: Decimal.Valid[P, S]
  ): CanTryCast[Float, Decimal[P, S]] = FloatToDecimal()
  given doubleToDecimal[P <: Int, S <: Int](using
      valid: Decimal.Valid[P, S]
  ): CanTryCast[Double, Decimal[P, S]] = DoubleToDecimal()

  given decimalToDecimal[P1 <: Int, S1 <: Int, P2 <: Int, S2 <: Int](using
      valid: Decimal.Valid[P2, S2]
  ): CanTryCast[Decimal[P1, S1], Decimal[P2, S2]] = DecimalToDecimal()

  given stringToByte: CanTryCast[String, Byte] = StringToByte
  given stringToShort: CanTryCast[String, Short] = StringToShort
  given stringToInt: CanTryCast[String, Int] = StringToInt
  given stringToLong: CanTryCast[String, Long] = StringToLong
}
