package com.biosimilarity.evaluator.spray

import java.io.InputStream
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.util._
import spray.io.{SSLContextProvider, ServerSSLEngineProvider}

object SSLConfiguration {

  private def sslContext: SSLContext = {
    val keystoreResource: InputStream = resourceStream("keystore.jks")
    try {
      val storepass: String                        = EvalConfigWrapper.readString("storepass")
      val keypass: String                          = EvalConfigWrapper.readString("keypass")
      val keyStore: KeyStore                       = KeyStore.getInstance("jks")
      val keyManagerFactory: KeyManagerFactory     = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      val trustManagerFactory: TrustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      val context: SSLContext                      = SSLContext.getInstance("TLS")
      keyStore.load(keystoreResource, storepass.toCharArray)
      keyManagerFactory.init(keyStore, keypass.toCharArray)
      trustManagerFactory.init(keyStore)
      context.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, new SecureRandom)
      context
    } finally {
      keystoreResource.close()
    }
  }

  def sslEngineProvider: ServerSSLEngineProvider = ServerSSLEngineProvider(identity)(SSLContextProvider.forContext(sslContext))
}
