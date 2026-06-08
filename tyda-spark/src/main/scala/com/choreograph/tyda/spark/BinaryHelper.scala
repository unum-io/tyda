package com.choreograph.tyda.spark

import com.choreograph.tyda.Binary

private[spark] object BinaryHelper {
  def fromArray(bytes: Array[Byte]): Binary = Binary.fromArray(bytes)
  def toArray(b: Binary): Array[Byte] = b.toArray
}
