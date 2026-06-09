package com.choreograph.tyda

import java.nio.charset.StandardCharsets

import scala.collection.immutable.ArraySeq

/** An immutable sequence of bytes with value semantics.
  *
  * In SQL backends this corresponds to the `BINARY` / `BYTES` type.
  */
opaque type Binary = ArraySeq.ofByte

object Binary {

  /** An empty `Binary` value. */
  val empty: Binary = new ArraySeq.ofByte(Array.emptyByteArray)

  /** Create a `Binary` from an `Array[Byte]`.
    *
    * The array is copied so that subsequent mutations do not affect the result.
    */
  def fromArray(bytes: Array[Byte]): Binary = new ArraySeq.ofByte(bytes.clone())

  /** Encode a `String` to its UTF-8 byte representation. */
  def fromString(str: String): Binary = new ArraySeq.ofByte(str.getBytes(StandardCharsets.UTF_8))

  extension (b: Binary) {

    /** Returns the number of bytes. */
    def length: Int = b.length
  }

  private object BinaryToArray extends Injection[Binary, Array[Byte]] {
    def apply(b: Binary): Array[Byte] = b.toArray
    def invert(arr: Array[Byte]): Binary = new ArraySeq.ofByte(arr)
  }

  given codec: Codec[Binary] = Codec.fromInjection(BinaryToArray, Codec.Bytes)

  given arbitrary: Arbitrary[Binary] =
    for {
      size <- Arbitrary.between(0, 20)
      arr <- Arbitrary.bytes(size)
    } yield ArraySeq.ofByte(arr)

  given Groupable[Binary] = Groupable.derived
}
