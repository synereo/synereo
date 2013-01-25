package com.protegra_ati.agentservices.store.extensions

import java.net.URI
import com.biosimilarity.lift.lib.moniker._

object URIExtensions
{
  implicit def uriExt(source: URI) = new URIExt(source)

  class URIExt(source: URI) {

     def withPort(port: Int) : URI = {
      new URI(
        source.getScheme(),
        source.getUserInfo(),
        source.getHost(),
        port,
        source.getPath(),
        source.getQuery(),
        source.getFragment()
      )
    }

     def withPath(path: String) : URI = {
      new URI(
        source.getScheme(),
        source.getUserInfo(),
        source.getHost(),
        source.getPort(),
        path,
        source.getQuery(),
        source.getFragment()
      )
    }
  }
}

object URMExtensions
{
  implicit def urmExt(source: URM) = new URMExt(source)

  class URMExt(source: URM) {

     def withPort(port: Int) : URM = {
      new URM(
        source.getScheme,
        Some(source.getUserInfo),
        Some(source.getAuthority),
        source.getHost,
        Some(port),
        source.getPath,
        Some(source.getQuery),
        Some(source.getFragment)
      )
    }

     def withPath(path: String) : URM = {
      new URM(
        source.getScheme,
        Some(source.getUserInfo),
        Some(source.getAuthority),
        source.getHost,
        Some(source.getPort),
        path,
        Some(source.getQuery),
        Some(source.getFragment)
      )
    }
  }
}

object MonikerExtensions
{
  implicit def monikerExt(source: Moniker) = new MonikerExt(source)

  class MonikerExt(source: Moniker) {

     def withPort(port: Int) : Moniker = {
      new URM(
        source.getScheme,
        Some(source.getUserInfo),
        Some(source.getAuthority),
        source.getHost,
        Some(port),
        source.getPath,
        Some(source.getQuery),
        Some(source.getFragment)
      )
    }

     def withPath(path: String) : Moniker = {
      new URM(
        source.getScheme,
        Some(source.getUserInfo),
        Some(source.getAuthority),
        source.getHost,
        Some(source.getPort),
        path,
        Some(source.getQuery),
        Some(source.getFragment)
      )
    }
  }
}
