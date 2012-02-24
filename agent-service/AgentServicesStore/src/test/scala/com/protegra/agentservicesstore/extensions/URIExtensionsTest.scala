package com.protegra.agentservicesstore.extensions

/* User: jklassen
*/


import org.specs._
import java.net.URI
import org.specs.runner._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import com.protegra.agentservicesstore.extensions.URIExtensions._

class URIExtensionsTest
extends JUnit4(URIExtensionsTestSpecs)

object URIExtensionsTestSpecsRunner
extends ConsoleRunner(URIExtensionsTestSpecs)

object URIExtensionsTestSpecs extends Specification {
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
