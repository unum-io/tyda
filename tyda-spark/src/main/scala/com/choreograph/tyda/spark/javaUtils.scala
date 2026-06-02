package com.choreograph.tyda.spark

import javax.lang.model.SourceVersion
import org.apache.commons.lang3.reflect.MethodUtils

private def isValidJavaAccessor(cls: Class[?], name: String): Boolean =
  !SourceVersion.isKeyword(name) && SourceVersion.isIdentifier(name) &&
    MethodUtils.getMatchingAccessibleMethod(cls, name) != null
