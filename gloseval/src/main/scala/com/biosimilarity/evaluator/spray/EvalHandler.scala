// -*- mode: Scala;-*- 
// Filename:    EvalHandler.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 15 13:53:55 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import akka.actor._
import spray.routing._
import directives.CompletionMagnet
import spray.http._
import spray.http.StatusCodes._
import MediaTypes._

import spray.httpx.encoding._

import org.json4s._
import org.json4s.native.JsonMethods._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import javax.crypto._
import javax.crypto.spec.SecretKeySpec

import java.util.Date
import java.util.UUID

object CompletionMapper {
  import DSLCommLink.mTT
  @transient
  val map = new HashMap[String,HttpService]()
  def complete( key : String, message : String ) : Unit = {
    println("CompletionMapper complete key="+key+", message="+message)
    for( srvc <- map.get( key ) ) {
      srvc.complete(HttpResponse(200, message))
    }
  }
}

trait EvalHandler {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT

  @transient
  implicit val formats = DefaultFormats

  def createUserRequest(json : JValue, srvcKey : String) = {
    val email = (json \ "content" \ "email").extract[String]
    val password = (json \ "content" \ "password").extract[String]
    val sessionID = UUID.randomUUID
    println("createUserRequest sessionID = " + sessionID.toString)
    val erql = agentMgr().erql( sessionID )
    val erspl = agentMgr().erspl( sessionID ) 
    
    def complete(capAndMac: Either[String, String]): Unit = {
      println("createUserRequest complete capAndMac="+capAndMac)
      val body = capAndMac match {
        // TODO(mike): use escaping interpolation
        case Left(cap) => "{\"msgType\": \"createUserResponse\", \"content\": {\"agentURI\": \"agent://" + 
            cap + "\"}}"
        case Right(reason) => "{\"msgType\": \"createUserError\", \"content\": {\"reason\": \"" + 
            reason + "\"}}"
      }
      println("createUserRequest complete body="+body)
      CompletionMapper.complete( srvcKey, body )
    }
    println("createUserRequest calling agentMgr().secureSignup")
    agentMgr().secureSignup(erql, erspl)(email, password, complete)
    println("createUserRequest return from secureSignup call")
  }
  

  def initializeSessionRequest(
    json : JValue,
    srvcKey : String,
    onPost : Option[Option[mTT.Resource] => Unit] = None
    //( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
  ) = {
    val agentURI = (json \ "content" \ "agentURI").extract[String]
    val uri = new java.net.URI(agentURI)

    if (uri.getScheme() != "agent") {
      throw InitializeSessionException(agentURI, "Unrecognized scheme")
    }
    val identType = uri.getHost()
    val identInfo = uri.getPath.substring(1) // drop leading slash
    // TODO: get a proper library to do this
    val queryMap = new HashMap[String, String]
    uri.getRawQuery.split("&").map((x: String) => {
      val pair = x.split("=")
      queryMap += ((pair(0), pair(1)))
    })
    var password = queryMap.get("password").getOrElse("")
    
    val sessionID = UUID.randomUUID
    val erql = agentMgr().erql( sessionID )
    val erspl = agentMgr().erspl( sessionID ) 

    def complete(message: String): Unit = {
      CompletionMapper.complete( srvcKey, message )
    }

    // (str:String) => complete(HttpResponse(entity = HttpBody(`application/json`, str)))
    agentMgr().secureLogin(erql, erspl)(
      identType, 
      identInfo, 
      password,
      complete
    )
  }

  def evalSubscribeRequest(json: JValue) : java.net.URI = {
    import com.biosimilarity.evaluator.distribution.portable.v0_1._
    // val evalSubscribeRequest( sessionURI, expression ) =
    //   ( json \ "content" ).extract[evalSubscribeRequest]
    val sessionURIstr = (json \ "content" \ "sessionURI").extract[String]
    val sessionURI = new java.net.URI(sessionURIstr)
    // TODO(mike): Tag expression objects so that we can pick the right case
    //   class in the match block below.
    // val expression = (json \ "content" \ "expression").extract[???]

    // if (sessionURI != "agent-session://ArtVandelay@session1") {
    //   throw EvalException(sessionURI)
    // }

    val sessionID = UUID.randomUUID
    val erql = agentMgr().erql( sessionID )
    val erspl = agentMgr().erspl( sessionID ) 

    /*
    expression match {
      case ConcreteHL.FeedExpr( filter, cnxns ) => {                    
        agentMgr().feed( erql, erspl )( filter, cnxns )
      }
      case ConcreteHL.ScoreExpr( filter, cnxns, staff ) => {
        agentMgr().score( erql, erspl )( filter, cnxns, staff )
      }
      case ConcreteHL.InsertContent( filter, cnxns, content : String ) => {
        agentMgr().post[String]( erql, erspl )( filter, cnxns, content )
      }
    }
    //*/

    sessionURI
  }  

  def connectServers( srvcKey : String, sessionId : UUID ) : Unit = {
    connectServers( sessionId )(
      ( optRsrc : Option[mTT.Resource] ) => {
        println( "got response: " + optRsrc )
        CompletionMapper.complete( srvcKey, optRsrc.toString )
      }
    )    
  }

  def connectServers( sessionId : UUID )(
    onConnection : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
  ) : Unit = {
    val pulseErql = agentMgr().adminErql( sessionId )
    val pulseErspl = agentMgr().adminErspl( sessionId )
    ensureServersConnected(
      pulseErql,
      pulseErspl
    )(
      onConnection
    )
  }

  def sessionPing(json: JValue) : String = {
    val sessionURI = (json \ "content" \ "sessionURI").extract[String]
    // TODO: check sessionURI validity
    
    sessionURI
  }



  def closeSessionRequest(json: JValue) : (String, spray.http.HttpBody) = {
    val sessionURI = (json \ "content" \ "sessionURI").extract[String]
    if (sessionURI != "agent-session://ArtVandelay@session1") {
      throw CloseSessionException(sessionURI, "Unknown session.")
    }

    (sessionURI, HttpBody(`application/json`,
      """{
        "msgType": "closeSessionResponse",
        "content": {
          "sessionURI": "agent-session://ArtVandelay@session1",
        }
      }
      """
    ))
  }

}

object EvalHandlerService
extends EvalHandler
with EvaluationCommsService
with EvalConfig
with DSLCommLinkConfiguration
with Serializable {
}
