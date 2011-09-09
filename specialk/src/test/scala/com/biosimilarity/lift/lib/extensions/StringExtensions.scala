package com.biosimilarity.lift.lib.extensions

import java.net.URI
import com.biosimilarity.lift.lib.moniker._

object StringExtensions
{
  implicit def stringExt(s: String) = new StringExt(s)

  class StringExt(source: String) {

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