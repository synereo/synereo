package com.protegra_ati.agentservices.core.schema.util

object PrologFormatter
{
  final val stripRegex = """[^a-zA-Z0-9 \[\]\-\:\.\@\,]""".r

  def clean(source: String): String =
  {
    source match {
      case null => null
      case s:String => stripRegex.replaceAllIn(source, " ")
    }
  }

}
