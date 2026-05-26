/* Licensed to the Apache Software Foundation (ASF) under one or more contributor license agreements. See the
 * NOTICE file distributed with this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License. */
package com.choreograph.tyda

/** the code here is basically copied from spark's externalcatalogutils, which
  * is licensed under the apache license, version 2.0.
  * https://github.com/apache/spark/blob/v4.1.1/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/catalog/externalcatalogutils.scala
  *
  * The motivation is that we want to write partition values that are compatible
  * with spark (and hopefully other tools in the ecosystem) without having
  * dependency on spark. the code is copied and has minor modifications for
  * scala 3.
  *
  * There 2 main reasons we can not use `java.net.URLEncoder` is that it encodes
  * space as `+` instead of `%20` (this could be fixed by a second enconding
  * step). But it also encodes multi-byte characters, which is not something the
  * spark unscape logic handles correctly. This multi-byte encoding is part of
  * most url encoding schemes, so we will run into that issue even if using
  * apache commons codec or guava.
  */
private[tyda] object PartitionEncoding {

  private val (charToEscape, sizeOfCharToEscape) = {
    val bitSet = new java.util.BitSet(128)

    /** ASCII 01-1F are HTTP control characters that need to be escaped. \u000A
      * and \u000D are \n and \r, respectively.
      */
    val clist = Array(
      '\u0001', '\u0002', '\u0003', '\u0004', '\u0005', '\u0006', '\u0007', '\u0008', '\u0009', '\n',
      '\u000B', '\u000C', '\r', '\u000E', '\u000F', '\u0010', '\u0011', '\u0012', '\u0013', '\u0014',
      '\u0015', '\u0016', '\u0017', '\u0018', '\u0019', '\u001A', '\u001B', '\u001C', '\u001D', '\u001E',
      '\u001F', '"', '#', '%', '\'', '*', '/', ':', '=', '?', '\\', '\u007F', '{', '[', ']', '^'
    )

    clist.foreach(bitSet.set(_))

    (bitSet, bitSet.size)
  }

  private final val HEX_CHARS = "0123456789ABCDEF".toCharArray

  private inline def needsEscaping(c: Char): Boolean = c < sizeOfCharToEscape && charToEscape.get(c)

  def encode(path: String): String = {
    val length = path.length
    var firstIndex = 0
    while (firstIndex < length && !needsEscaping(path.charAt(firstIndex))) firstIndex += 1
    if (firstIndex == length) { path }
    else {
      val sb = new java.lang.StringBuilder(length + 16)
      if (firstIndex != 0) sb.append(path, 0, firstIndex): Unit
      while (firstIndex < length) {
        val c = path.charAt(firstIndex)
        if (needsEscaping(c)) {
          sb.append('%').append(HEX_CHARS((c & 0xf0) >> 4)).append(HEX_CHARS(c & 0x0f))
        } else { sb.append(c) }
        firstIndex += 1
      }
      sb.toString
    }
  }

  private val unhexDigits = {
    val array = Array.fill[Byte](128)(-1)
    (0 to 9).foreach(i => array('0' + i) = i.toByte)
    (0 to 5).foreach(i => array('A' + i) = (i + 10).toByte)
    (0 to 5).foreach(i => array('a' + i) = (i + 10).toByte)
    array
  }

  def decode(path: String): String = {
    if (path == null || path.isEmpty) { return path }
    var plaintextEndIdx = path.indexOf('%')
    val length = path.length
    if (plaintextEndIdx == -1 || plaintextEndIdx + 2 >= length) {
      // fast path, no %xx encoding found then return the string identity
      path
    } else {
      val sb = new java.lang.StringBuilder(length)
      var plaintextStartIdx = 0
      while (plaintextEndIdx != -1 && plaintextEndIdx + 2 < length) {
        if (plaintextEndIdx > plaintextStartIdx) sb.append(path, plaintextStartIdx, plaintextEndIdx): Unit
        val high = path.charAt(plaintextEndIdx + 1)
        if ((high >>> 8) == 0 && unhexDigits(high) != -1) {
          val low = path.charAt(plaintextEndIdx + 2)
          if ((low >>> 8) == 0 && unhexDigits(low) != -1) {
            sb.append((unhexDigits(high) << 4 | unhexDigits(low)).toChar)
            plaintextStartIdx = plaintextEndIdx + 3
          } else {
            sb.append('%')
            plaintextStartIdx = plaintextEndIdx + 1
          }
        } else {
          sb.append('%')
          plaintextStartIdx = plaintextEndIdx + 1
        }
        plaintextEndIdx = path.indexOf('%', plaintextStartIdx)
      }
      if (plaintextStartIdx < length) { sb.append(path, plaintextStartIdx, length): Unit }
      sb.toString
    }
  }
}
