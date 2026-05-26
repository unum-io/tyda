package com.choreograph.tyda.table

import com.choreograph.tyda.TypeName

final case class SourceAndModelName[M](source: Source[M, ?], name: TypeName[M])
