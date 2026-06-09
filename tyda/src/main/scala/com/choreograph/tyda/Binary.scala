package com.choreograph.tyda

import java.nio.charset.StandardCharsets
import java.util.Arrays
import java.util.Base64

import scala.collection.Factory
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

  /** Decode a Base64-encoded string to binary data.
    *
    * Returns `None` if the string is not valid Base64. This follows common sql
    * semantics and filters out whitespace before trying to decode.
    */
  def fromBase64(str: String): Option[Binary] =
    try Some(new ArraySeq.ofByte(Base64.getDecoder.decode(str.filter(!_.isWhitespace))))
    catch { case _: IllegalArgumentException => None }

  extension (b: Binary) {

    /** Returns the number of bytes. */
    def length: Int = b.length

    /** Encode binary data as a Base64 string. */
    def toBase64: String = Base64.getEncoder.encodeToString(b.toArray)

    /** Returns a copy of the bytes as C. */
    def to[C](factory: Factory[Byte, C]): C = factory.fromSpecific(b)
  }

  given arbitrary: Arbitrary[Binary] =
    for {
      size <- Arbitrary.between(0, 20)
      arr <- Arbitrary.bytes(size)
    } yield ArraySeq.ofByte(arr)

  given Groupable[Binary] = Groupable.derived

  given Ordering[Binary] = (x, y) => Arrays.compareUnsigned(x.unsafeArray, y.unsafeArray)
}
