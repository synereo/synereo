/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.store.extensions

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
      val label = labelFromKey(source)
      if (label == null) {
        //there is something wrong with the data in the key....look for double quotes in the actual data fields esp as this causes
        // toLabel to return null or the wrong data.
        throw new NullPointerException("ERROR: toLabel method returned null")
      }
      label
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