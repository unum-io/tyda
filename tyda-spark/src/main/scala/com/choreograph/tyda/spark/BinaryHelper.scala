package com.choreograph.tyda.spark

import scala.collection.immutable.ArraySeq

private[spark] object BinaryHelper {
  def fromArray(bytes: Array[Byte]): ArraySeq.ofByte = new ArraySeq.ofByte(bytes.clone())
}
