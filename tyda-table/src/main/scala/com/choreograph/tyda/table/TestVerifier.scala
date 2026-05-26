package com.choreograph.tyda.table

enum TestVerifier[M] {
  case Fixed(verifier: Seq[M] => Any)
  case Partitioned(pathToVerifier: Map[String, Seq[M] => Any])

  def getVerifier[P <: Partitioner](p: P): Seq[M] => Any =
    this match {
      case TestVerifier.Fixed(verify) => verify
      case TestVerifier.Partitioned(pathToVerifier) =>
        val path = p.path("/")
        val specifiedPaths = pathToVerifier.view.keys.mkString(", ")
        pathToVerifier.getOrElse(
          path,
          throw new RuntimeException(s"Missing verifier for path $path only specified $specifiedPaths")
        )
    }
}
