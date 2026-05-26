package com.choreograph.tyda

/** Spark and other query engines often support access to metadata like path,
  * modified time, and file sizes.
  *
  * This adds this information in a typed class. Currently, only the path is
  * provided as that is all that is currently needed.
  *
  * Note: This class is created using untyped apis in each tyda backend.
  * Therefore, each backend must be updated when extending this class.
  */
final case class FileMetadata(file_path: String) derives Codec
