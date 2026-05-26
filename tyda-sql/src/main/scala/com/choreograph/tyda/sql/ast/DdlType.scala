package com.choreograph.tyda.sql.ast

private[sql] final case class DdlField(
    name: Identifier,
    tpe: DdlType,
    nullable: Boolean,
    comment: Option[String] = None
)

private[sql] final case class TypeAndNullabilityAndComment(
    tpe: DdlType,
    nullable: Boolean,
    comment: Option[String] = None
) {
  def named(name: String): DdlField = DdlField(Identifier(name), tpe, nullable, comment)
}

private[sql] enum DdlType {
  case Primitive(name: String)
  case Array(element: TypeAndNullabilityAndComment)
  case Map(key: TypeAndNullabilityAndComment, value: TypeAndNullabilityAndComment)
  case Struct(fields: Seq[DdlField])
}
