package com.choreograph.tyda

import java.util.concurrent.atomic.AtomicLong

opaque type ReferenceId = Long

object ReferenceId {
  private val nextId = new AtomicLong(0)

  def apply(): ReferenceId = nextId.getAndIncrement()
}
