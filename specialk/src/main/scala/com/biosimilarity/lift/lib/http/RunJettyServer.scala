package com.biosimilarity.lift.lib.http

import java.io.File

import org.eclipse.jetty.server.Connector
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.nio.SelectChannelConnector
import org.eclipse.jetty.webapp.WebAppContext

/**

Run this via mvn with

mvn exec:java -Dexec.mainClass=com.biosimilarity.lift.lib.http.RunJettyServer


*/
object RunJettyServer extends App {

  org.apache.log4j.BasicConfigurator.configure()

  lazy val connectors = Array[Connector](httpConnector(8090))
  
  val webappDirectory = new File("./src/main/webapp/").getCanonicalFile
  
  if ( !webappDirectory.exists ) sys.error("webapp directory does not exist " + webappDirectory)

  val server = new Server()
  server.setConnectors(connectors)

  val webapp = new WebAppContext()
	webapp.setContextPath("/")
	webapp.setWar(webappDirectory.toURL().toExternalForm())
	webapp.setDefaultsDescriptor(getResource("webdefault.xml").toExternalForm())

  server.setHandler(webapp)

  server.start()
  server.join()

  def httpConnector(port: Int): SelectChannelConnector = {
    val c = new SelectChannelConnector()
    c.setPort(port)
    c
  }

  def getResource(resource: String): java.net.URL = {
    Thread.currentThread().getContextClassLoader().getResource(resource)
  }
    
}
