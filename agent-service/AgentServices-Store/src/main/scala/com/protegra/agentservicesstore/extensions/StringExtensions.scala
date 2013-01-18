/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra.agentservicesstore.extensions

import com.biosimilarity.lift.model.store._
import java.net.URI
import com.biosimilarity.lift.lib.moniker._

//don't import DirectedLinks.SpecialKURIDefaults, or AgentLinks.AgentURIDefaults when using this
object StringExtensions
extends CnxnXQueryParser
{
  final val separators = scala.Array('.', ',', '$', '-')

  implicit def stringExt(s: String) = new StringExt(s)

  class StringExt(source: String) {

    def toCamelCase: String = {
      source.substring(0,1).toLowerCase + source.substring(1)
    }
    def fromCamelCase:String = {
      source.substring(0,1).toUpperCase + source.substring(1)
    }


    def trimPackage: String = {
      if (source == null) return ""
      val words = source.split(separators)
      words.last
    }

    def toLabel: CnxnCtxtLabel[String,String,String] = {
      labelFromKey(source)
    }

    val scheme  = "agent"
    val path = ""
    val fragment = ""
    def toURI : URI = {
      new URI( scheme, source, path, fragment )
    }

    def toURM : URM = {
      new URM( scheme, source, path, Some(fragment) )
    }

    def short : String = {
      val max = if ( source.toString.length < 512 ) source.toString.length else 512
      source.toString.substring( 0, max )
    }
  }
}