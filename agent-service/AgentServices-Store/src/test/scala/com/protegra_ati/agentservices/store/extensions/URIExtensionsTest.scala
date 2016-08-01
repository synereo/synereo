package com.protegra_ati.agentservices.store.extensions

import java.net.URI

import com.protegra_ati.agentservices.store.extensions.StringExtensions._
import com.protegra_ati.agentservices.store.extensions.URIExtensions._
import org.scalatest.{MustMatchers, WordSpec}

class URIExtensionsTest extends WordSpec with MustMatchers {

  "withPort" should {
    "add port" in {
      val port: Int = 1234
      val uri: URI  = "localhost".toURI.withPort(port)
      uri.getPort must ===(port)
    }
  }

  "port" should {
    "default" in {
      val uri: URI = "localhost".toURI
      uri.getPort must ===(-1)
    }
  }
}
