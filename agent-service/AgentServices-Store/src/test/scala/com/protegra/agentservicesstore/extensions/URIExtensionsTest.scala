package com.protegra.agentservicesstore.extensions

/* User: jklassen
*/


import org.specs2.mutable._
import java.net.URI
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._

class URIExtensionsTest extends SpecificationWithJUnit {
   "withPort" should {
     "add port" in {
       val port = 1234
       val uri = "localhost".toURI.withPort(port)

       uri.getPort() must be_==(port)
     }
   }

  "port" should {
     "default" in {
       val uri = "localhost".toURI

       uri.getPort() must be_==(-1)
     }
   }
}
