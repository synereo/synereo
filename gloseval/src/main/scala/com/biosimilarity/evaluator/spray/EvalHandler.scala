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
import com.biosimilarity.evaluator.dsl.usage.ConcreteHL._
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
import org.json4s.JsonDSL._

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
  @transient
  val map = new HashMap[String, RequestContext]()
  def complete(key:String, message: String): Unit = {
    println("CompletionMapper complete key="+key+", message="+message)
    for (reqCtx <- map.get(key)) {
      println("CompletionMapper complete reqCtx="+reqCtx)
      reqCtx.complete(HttpResponse(200, message))
    }
    map -= key
  }
}

trait EvalHandler {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT

  @transient
  implicit val formats = DefaultFormats
  
  def createUserRequest(json : JValue, key : String): Unit = {
    val email = (json \ "content" \ "email").extract[String]
    val password = (json \ "content" \ "password").extract[String]
    val jsonBlob = compact(render(json \ "content" \ "jsonBlob"))
    
    def complete(capAndMac: Either[String, String]): Unit = {
      println("createUserRequest complete capAndMac="+capAndMac)
      val body = capAndMac match {
        // TODO(mike): use escaping interpolation
        case Left(cap) => "{\"msgType\": \"createUserResponse\", \"content\": {\"agentURI\": \"agent://cap/" +
            cap + "\"}}"
        case Right(reason) => "{\"msgType\": \"createUserError\", \"content\": {\"reason\": \"" +
            reason + "\"}}"
      }
      println("createUserRequest complete body="+body)
      CompletionMapper.complete( key, body )
    }
    println("createUserRequest calling agentMgr().secureSignup")
    agentMgr().secureSignup(email, password, jsonBlob, complete)
    println("createUserRequest return from secureSignup call")
  }
  

  def initializeSessionRequest(
    json : JValue,
    key : String,
    onPost : Option[Option[mTT.Resource] => Unit] = None
  ): Unit = {
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
    
    def complete(message: String): Unit = {
      CompletionMapper.complete( key, message )
    }

    // (str:String) => complete(HttpResponse(entity = HttpBody(`application/json`, str)))
    agentMgr().secureLogin(
      identType, 
      identInfo, 
      password,
      complete
    )
  }

  def evalSubscribeRequest(json: JValue, cometMessage: (String, String) => Unit) : Unit = {
    import com.biosimilarity.evaluator.distribution.portable.v0_1._

    val content = (json \ "content").asInstanceOf[JObject]
    val sessionURIstr = (content \ "sessionURI").extract[String]
    val (erql, erspl) = agentMgr().makePolarizedPair()
    // TODO(mike): Tag expression objects so that we can pick the right case
    //   class in the match block below.
    val exprType = (content \ "msgType").extract[String]
    exprType match {
      case "feedExpr" => {
        val feedExpr = (content \ "content").extract[com.biosimilarity.evaluator.distribution.portable.dsl.FeedExpr]
        val onFeed: Option[mTT.Resource] => Unit = (rsrc) => {
          rsrc match {
            case None => ()
            case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
              postedExpr.asInstanceOf[PostedExpr[String]] match {
                case PostedExpr(postedStr) => {
                  val content =
                    ("sessionURI" -> sessionURIstr) ~
                    ("pageOfPosts" -> List(postedStr))
                  val response = ("msgType" -> "evalSubscribeResponse") ~ ("content" -> content)
                  cometMessage(sessionURIstr, compact(render(response)))
                }
              }
            }
            case _ => throw new Exception("Unrecognized resource: " + rsrc)
          }
        }
        agentMgr().feed(erql, erspl)(feedExpr.filter, feedExpr.cnxns, onFeed)
      }
      case "scoreExpr" => {
        val scoreExpr = (content \ "content").extract[com.biosimilarity.evaluator.distribution.portable.dsl.ScoreExpr]
        val onScore: Option[mTT.Resource] => Unit = (rsrc) => {
          rsrc match {
            case None => ()
            case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
              postedExpr.asInstanceOf[PostedExpr[String]] match {
                case PostedExpr(postedStr) => {
                  val content =
                    ("sessionURI" -> sessionURIstr) ~
                    ("pageOfPosts" -> List(postedStr))
                  val response = ("msgType" -> "evalSubscribeResponse") ~ ("content" -> content)
                  cometMessage(sessionURIstr, compact(render(response)))
                }
              }
            }
            case _ => throw new Exception("Unrecognized resource: " + rsrc)
          }
        }
        agentMgr().score(erql, erspl)(scoreExpr.filter, scoreExpr.cnxns, scoreExpr.staff, onScore)
      }
      case "insertContent" => {
        val insertContent = (content \ "content").extract[com.biosimilarity.evaluator.distribution.portable.dsl.InsertContent[String]]
        val onPost: Option[mTT.Resource] => Unit = (rsrc) => {
          // evalComplete, empty seq of posts
          val content =
            ("sessionURI" -> sessionURIstr) ~
            ("pageOfPosts" -> List())
          val response = ("msgType" -> "evalComplete") ~ ("content" -> content)
          cometMessage(sessionURIstr, compact(render(response)))
        }
        agentMgr().post(erql, erspl)(insertContent.filter, insertContent.cnxns, insertContent.value, onPost)
      }
      case _ => {
        throw new Exception("Unrecognized request: " + compact(render(json)))
      }
    }
  }  

  def connectServers( key : String, sessionId : UUID ) : Unit = {
    connectServers( sessionId )(
      ( optRsrc : Option[mTT.Resource] ) => {
        println( "got response: " + optRsrc )
        CompletionMapper.complete( key, optRsrc.toString )
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
