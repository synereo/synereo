package com.protegra_ati.agentservices.core.util

import java.util.ArrayList
import java.util.List
import java.util.Scanner
import java.util.Collections

/* User: mgevantmakher
*/

object StringToListConverter
{
  def toJavaList(str: String): List[ String ] =
  {
    if ( null == str || str.isEmpty )
      return Collections.emptyList[ String ]()
    var st=str.replaceAll("[\\]+$\\[+$]","")

    val scanner = new Scanner(st).useDelimiter("\\s*,\\s*")
    val output: java.util.List[String] = new ArrayList[String]()
    while ( scanner.hasNext ) {output.add(scanner.next())}
    output
  }
}
