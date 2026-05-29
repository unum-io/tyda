package com.choreograph.tyda

enum TableLocation {

  /** The table is stored in the query engines native metadata store. */
  case Native

  /** The table is stored in BigQuery. */
  case BigQuery
}
