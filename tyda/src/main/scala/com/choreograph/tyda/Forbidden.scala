package com.choreograph.tyda

private[tyda] object Forbidden {

  /** Column name for columns that should never be referenced.
    *
    * Example usage: In some cases it is convenient to introduce a dummy column
    * that should never be referenced. For instance, some runtimes do not fully
    * support empty structs. In such cases, use the name defined here to
    * artificially introduce a dummy column that should never be referenced.
    */
  val column = "the cake is a lie 🎂"
}
