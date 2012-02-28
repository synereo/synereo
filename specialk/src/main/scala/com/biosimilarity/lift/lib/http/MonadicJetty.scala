// -*- mode: Scala;-*- 
// Filename:    MonadicJetty.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb  2 18:16:49 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import com.biosimilarity.lift.lib.moniker._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._

import com.rabbitmq.client.{ Channel => RabbitChan, _}

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver


import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.eclipse.jetty.server.session.HashSessionManager;
import org.eclipse.jetty.server.session.SessionHandler;

trait MonadicEmbeddedJetty[T]
 extends MonadicGenerators
  with FJTaskRunners {
   self : WireTap with Journalist =>          
    
    type Channel = HttpServletRequest
    type Payload = ClientRequestPair[T]

    trait ClientRequestPair[T] {
      def httpServletReq : HttpServletRequest
      def httpServletRsp : HttpServletResponse
      def stall : Unit = synchronized { this.wait }
      def unstall : Unit = synchronized { this.notifyAll }
    }

    trait ServerContextPair[T] {
      def server : Server
      def context : ServletContextHandler            
    }

    case class CnR[T](
      override val server : Server,
      override val context : ServletContextHandler
    ) extends ServerContextPair[T]

    def acceptConnections(
      port : Int,
      path : String
    ) =
    Generator {
      k : ( CnR[T] => Unit @suspendable ) => {
	val server = new Server( port )
	val context = 
	  new ServletContextHandler( ServletContextHandler.SESSIONS )
	val sm = new HashSessionManager()
	val sh = new SessionHandler( sm )

	context.setContextPath( path )
	server.setHandler( context )	
	context.setSessionHandler( sh )

	server.start()
	spawn{ server.join() }

	k( CnR[T]( server, context ) )
      }
    }   

    def beginService(
      port : Int,
      path : String
    ) = {
      serve [T]( port, path )
    }

   def serve [T] (
     port : Int,
     path : String
   ) = Generator {
     k : ( Payload => Unit @suspendable ) =>
       tweet(
	 "The client is running... (don't let him get away!)"
       )

     for( cnr <- acceptConnections( port, path ) ) {
       spawn {
	 tweet( "Connected: " + cnr.server )
	 
         for ( t <- read [T] ( cnr.server, cnr.context ) ) { k( t ) }
       }
     }
   }

  def callbacks( srvr : Server, ctxt : ServletContextHandler ) =
    Generator {
      k : ( Payload => Unit @suspendable) =>

      tweet("level 1 callbacks")

      shift {
  	outerk : (Unit => Any) =>
	  
	  object TheRendezvous
	   extends HttpServlet with ServerContextPair[T] {
	     override def server = srvr
	     override def context = ctxt
    	     override def doGet(
	       req : HttpServletRequest,
	       rsp : HttpServletResponse
	     ) : Unit = {
	       val crp = 
		 new ClientRequestPair[T] {
		   override def httpServletReq = req
		   override def httpServletRsp = rsp
		 }
    	       
	       spawn { 
  		 tweet("before continuation in callback")		 
  		
    		 k( crp )
    		
    		 tweet("after continuation in callback")
    		   
		 outerk()
    	       }

	       crp.stall
    	     }	     
	   }
  	
  	tweet("before registering callback")
  	
	ctxt.addServlet( new ServletHolder( TheRendezvous ), "/*" )
  	
  	tweet("after registering callback")
  	// stop
      }
    }       

   def read [T] ( server : Server, context : ServletContextHandler ) =
     Generator {
       k: ( Payload => Unit @suspendable) =>
	 shift {
	   outerk: (Unit => Unit) =>
	     reset {
	      
  	       for (
		 reqRspPair <- callbacks( server, context )
	       ) {
		 k( reqRspPair )
		 
		 // Is this necessary?
		 shift { k : ( Unit => Unit ) => k() }
  	       }
  	       
  	       tweet( "readT returning" )
  	       outerk()
	     }
	 }
     }
   
 }
