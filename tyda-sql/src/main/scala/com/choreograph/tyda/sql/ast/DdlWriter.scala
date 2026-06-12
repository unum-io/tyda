package com.choreograph.tyda.sql.ast

import java.io.Writer

import com.choreograph.tyda.sql.ast.SqlKeywords.isKeyword

private[sql] final case class DdlWriter(writer: Writer, pretty: Boolean) {
  private def breakableSpace(depth: Int): Unit =
    if pretty then {
      writer.write("\n")
      (0 until depth).foreach(_ => writer.write("  "))
    } else { writer.write("") }
  private def writeNullable(nullable: Boolean): Unit = if !nullable then writer.write(" NOT NULL")
  private def writeComment(comment: String): Unit =
    if pretty then {
      writer.write(" /*")
      writer.write(comment)
      writer.write("*/")
    }

  def write(ident: Identifier): Unit = DdlWriter.writeIdentifier(writer, ident)

  def write(fields: Seq[DdlField], depth: Int = 0): Unit =
    fields
      .zipWithIndex
      .foreach((field, idx) =>
        if idx > 0 then writer.write(",")
        breakableSpace(depth)
        write(field, depth)
      )

  def write(field: DdlField, depth: Int): Unit = {
    write(field.name)
    writer.write(" ")
    write(field.tpe, depth)
    writeNullable(field.nullable)
    field.comment.foreach(writeComment)
  }

  def write(field: TypeAndNullabilityAndComment, depth: Int): Unit = {
    write(field.tpe, depth)
    writeNullable(field.nullable)
    field.comment.foreach(writeComment)
  }

  def write(tpe: DdlType, depth: Int): Unit =
    tpe match {
      case DdlType.Primitive(name) => writer.write(name)
      case DdlType.Array(element) =>
        writer.write("ARRAY<")
        write(element, depth)
        writer.write(">")
      case DdlType.Map(key, value) =>
        writer.write("MAP<")
        write(key, depth)
        writer.write(", ")
        write(value, depth)
        writer.write(">")
      case DdlType.Struct(fields) =>
        writer.write("STRUCT<")
        write(fields, depth + 1)
        breakableSpace(depth)
        writer.write(">")
    }
}

private[sql] object DdlWriter {
  extension (c: Char) {
    def isAsciiLetterOrDigit: Boolean =
      (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9')
  }
  def shouldQuoteIdentifier(ident: Identifier): Boolean = {
    val name = ident.value
    name.exists(c => !c.isAsciiLetterOrDigit && c != '_') || name.headOption.exists(_.isDigit) ||
    isKeyword(name)
  }

  def writeIdentifier(writer: Writer, ident: Identifier): Unit =
    if shouldQuoteIdentifier(ident) then {
      writer.write('`')
      ident
        .value
        .foreach {
          case '`' => writer.write("``")
          case c => writer.write(c)
        }
      writer.write('`')
    } else { writer.write(ident.value) }
}
