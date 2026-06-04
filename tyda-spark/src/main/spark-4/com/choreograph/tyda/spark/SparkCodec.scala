package com.choreograph.tyda.spark

type SparkCodec[F, T] = org.apache.spark.sql.catalyst.encoders.Codec[F, T]
