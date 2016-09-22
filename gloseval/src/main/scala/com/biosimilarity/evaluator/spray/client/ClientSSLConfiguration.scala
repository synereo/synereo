package com.biosimilarity.evaluator.spray.client

import java.io.InputStream
import java.security.cert.{Certificate, CertificateFactory}
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{SSLContext, SSLParameters, TrustManagerFactory}

import com.biosimilarity.evaluator.util._
import spray.io.{ClientSSLEngineProvider, SSLContextProvider}

object ClientSSLConfiguration {

  private def loadX509Certificate(resourceName: String): Certificate = {
    val certResource: InputStream = resourceStream(resourceName)
    try {
      CertificateFactory.getInstance("X.509").generateCertificate(certResource)
    } finally {
      certResource.close()
    }
  }

  private def clientSSLContext: SSLContext = {
    val keystore: KeyStore                       = KeyStore.getInstance(KeyStore.getDefaultType)
    val trustManagerFactory: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    val context: SSLContext                      = SSLContext.getInstance("TLS")
    val params: SSLParameters                    = new SSLParameters
    keystore.load(null, null)
    keystore.setCertificateEntry("ca", loadX509Certificate("gloseval.pem"))
    trustManagerFactory.init(keystore)
    context.init(null, trustManagerFactory.getTrustManagers, new SecureRandom)
    params.setEndpointIdentificationAlgorithm("https")
    context
  }

  def clientSSLEngineProvider: ClientSSLEngineProvider = ClientSSLEngineProvider(identity)(SSLContextProvider.forContext(clientSSLContext))
}
