package com.biosimilarity.lift.lib.extensions

import java.net.URI
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.model.store._

object StringExtensions
  extends CnxnXQueryParser
{
  implicit def stringExt(s: String) = new StringExt(s)

  class StringExt(source: String) {

    def toLabel: CnxnCtxtLabel[String,String,String] = {
      labelFromKey(source)
    }

    val scheme  = "agent"
    val path = "/invitation"
    val fragment = ""
    def toURI : URI = {
      new URI( scheme, source, path, fragment )
    }

    def toURM : URM = {
      new URM( scheme, source, path, Some(fragment) )
    }

  }
}