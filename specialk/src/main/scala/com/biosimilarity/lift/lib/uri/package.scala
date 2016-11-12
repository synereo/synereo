package com.biosimilarity.lift.lib

import java.net.{InetSocketAddress, URI}

package object uri {

  def parseInetSocketAddress(s: String, defaultHost: String, defaultPort: Int): InetSocketAddress = {
    val uri: URI     = new URI("unused://" + s.trim)
    val host: String = Option(uri.getHost).getOrElse(defaultHost)
    val port: Int = uri.getPort match {
      case -1 => defaultPort
      case x  => x
    }
    new InetSocketAddress(host, port)
  }
}
