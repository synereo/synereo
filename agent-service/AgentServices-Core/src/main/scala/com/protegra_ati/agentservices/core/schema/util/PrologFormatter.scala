package com.protegra_ati.agentservices.core.schema.util

object PrologFormatter
{
  def clean(source: String): String =
  {
    //TODO: remove this temp hack once kvdb resolves 326
    //reserved chars: #, $, &, *, +, -, ., /, :, <, =, >, ?, @, ^, ~
    //can't replace -, GUIDs need this and KVDB protects against this already

    val clean = source.replace("#", " ")
      .replace("$", " ")
      .replace("&", " ")
      .replace("*", " ")
      .replace("+", " ")
      .replace(".", " ")
      .replace("/", " ")
      .replace(":", " ")
      .replace("<", " ")
      .replace("=", " ")
      .replace(">", " ")
      .replace("?", " ")
      .replace("@", " ")
      .replace("^", " ")
      .replace("~", " ")

    clean
//    source
  }

}
