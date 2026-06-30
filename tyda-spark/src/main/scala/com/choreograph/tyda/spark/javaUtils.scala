package com.choreograph.tyda.spark

import java.lang.reflect.TypeVariable

import javax.lang.model.SourceVersion
import org.apache.commons.lang3.reflect.MethodUtils

import com.choreograph.tyda.Codec
import com.choreograph.tyda.unreachable

private def isValidJavaAccessor(cls: Class[?], name: String): Boolean =
  !SourceVersion.isKeyword(name) && SourceVersion.isIdentifier(name) &&
    MethodUtils.getMatchingAccessibleMethod(cls, name) != null

private def valueClassFieldWouldBeBoxedAtRuntime[T](prod: Codec[T], fieldName: String): Boolean = {
  val clazz = prod.classTag.runtimeClass
  clazz.getMethods().filter(m => m.getName() == fieldName) match {
    case Array(single) =>
      // When values classes are treated as a different type (which includes generics) the value is boxed
      single.getGenericReturnType() match {
        case _: TypeVariable[?] => true
        case _ => false
      }
    case methods => unreachable(s"Multiple or no accessors found for field ${fieldName} on $prod: $methods")
  }
}
