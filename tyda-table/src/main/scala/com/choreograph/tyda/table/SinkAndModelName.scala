package com.choreograph.tyda.table

import com.choreograph.tyda.TypeName

final case class SinkAndModelName[M](sink: Sink[M, ?], name: TypeName[M])
