package com.protegra_ati.agentservices.core.extensions

import scala.language.implicitConversions

object ClassExtensions
{
  implicit def classExt[ T <: AnyRef ](x: T) = new ClassExt(x)

  class ClassExt[ T <: AnyRef ](x: T)
  {
    def getClassOf: Class[ _ <: T ] = x.getClass.asInstanceOf[ Class[ T ] ]
  }
}
