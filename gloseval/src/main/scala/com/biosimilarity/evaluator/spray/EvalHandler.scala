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
import java.security._


import java.util.Date
import java.util.UUID

import java.net.URI

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

object ConfirmationEmail {
  def confirm(email: String, token: String) = {
    import org.apache.commons.mail._
    val simple = new SimpleEmail()
    simple.setHostName("smtp.googlemail.com")
    simple.setSmtpPort(465)
    simple.setAuthenticator(new DefaultAuthenticator("individualagenttech", "4genttech"))
    simple.setSSLOnConnect(true)
    simple.setFrom("individualagenttech@gmail.com")
    simple.setSubject("Confirm individual agent signup")
    // TODO(mike): get the URL from a config file
    simple.setMsg("""Please click on the following link to confirm that you'd like to create a new individual agent:
      http://64.27.3.17:6080/agentui.html?demo=false&token=""" + token)
    simple.addTo(email)
    simple.send()
  }
}

trait EvalHandler {
  self : EvaluationCommsService =>
 
  import DSLCommLink.mTT

  @transient
  implicit val formats = DefaultFormats
  
  val userDataFilter = fromTermString(
      //"userData(listOfAliases(A), defaultAlias(DA), listOfLabels(L), listOfCnxns(C), lastActiveFilter(F))"
      "userData(X)"
    ).getOrElse(throw new Exception("Couldn't parse userDataFilter"))
  val pwmacFilter = fromTermString("pwmac(X)").getOrElse(throw new Exception("Couldn't parse pwmacFilter"))
  val emailFilter = fromTermString("email(X)").getOrElse(throw new Exception("Couldn't parse emailFilter"))
  val tokenFilter = fromTermString("token(X)").getOrElse(throw new Exception("Couldn't parse tokenFilter"))

  def confirmEmailToken(token: String, key: String): Unit = {
    val tokenUri = new URI("token://" + token)
    val tokenCnxn = PortableAgentCnxn(tokenUri, "token", tokenUri)
    
    val (erql, erspl) = agentMgr().makePolarizedPair()
    // TODO(mike): change from fetch to a consuming verb
    agentMgr().fetch(erql, erspl)(tokenFilter, List(tokenCnxn), (rsrc: Option[mTT.Resource]) => {
      rsrc match {
        case None => ()
        case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
          postedExpr.asInstanceOf[PostedExpr[String]] match {
            case PostedExpr(postedStr) => {
              val content = parse(postedStr)
              val email = (content \ "email").extract[String]
              val password = (content \ "password").extract[String]
              val jsonBlob = compact(render(content \ "jsonBlob"))
              secureSignup(email, password, jsonBlob, key)
            }
          }
        }
        case _ => throw new Exception("Unrecognized resource: " + rsrc)
      }
    })
  }
  
  def emailToCap(email: String): String = {
    val macInstance = Mac.getInstance("HmacSHA256")
    macInstance.init(new SecretKeySpec("emailmac".getBytes("utf-8"), "HmacSHA256"))
    macInstance.doFinal(email.getBytes("utf-8")).map("%02x" format _).mkString.substring(0,36)
  }

  def storeCapByEmail(email: String): String = {
    // If email is nonempty, hash it for the cap part
    val cap = emailToCap(email)
    val emailURI = new URI("emailhash://" + cap)
    val emailSelfCnxn = //new ConcreteHL.PortableAgentCnxn(emailURI, emailURI.toString, emailURI)
      PortableAgentCnxn(emailURI, "emailhash", emailURI)
    val (erql, erspl) = agentMgr().makePolarizedPair()
    agentMgr().post[String](erql, erspl)(
      emailFilter,
      List(emailSelfCnxn),
      cap
    )
    cap
  }
  
  def secureSignup(
    email: String,
    password: String,
    jsonBlob: String,
    key: String
  ) : Unit = {
    import DSLCommLink.mTT
    val cap = if (email == "") UUID.randomUUID.toString else storeCapByEmail(email)
    BasicLogService.tweet("secureSignup email="+email+", password="+password+", cap="+cap)
    val macInstance = Mac.getInstance("HmacSHA256")
    macInstance.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
    val mac = macInstance.doFinal(cap.getBytes("utf-8")).slice(0,5).map("%02x" format _).mkString
    val capAndMac = cap + mac
    val capURI = new URI("usercap://" + cap)
    val capSelfCnxn = PortableAgentCnxn(capURI, "pwdb", capURI)

    macInstance.init(new SecretKeySpec("pAss#4$#".getBytes("utf-8"), "HmacSHA256"))
    val pwmac = macInstance.doFinal(password.getBytes("utf-8")).map("%02x" format _).mkString

    BasicLogService.tweet("secureSignup posting pwmac")
    val (erql, erspl) = agentMgr().makePolarizedPair()
    agentMgr().post[String](erql, erspl)(
      pwmacFilter,
      List(capSelfCnxn),
      pwmac,
      ( dummy : Option[mTT.Resource] ) => {
        BasicLogService.tweet("secureSignup onPost1")
        // Change String to Term throughout.
        val (erql, erspl) = agentMgr().makePolarizedPair()
        agentMgr().post[String](erql, erspl)(
          userDataFilter,
          List(capSelfCnxn),
          // "userData(listOfAliases(), defaultAlias(\"\"), listOfLabels(), " +
          //     "listOfCnxns(), lastActiveFilter(\"\"))",
          jsonBlob,
          ( dummy : Option[mTT.Resource] ) => {
            BasicLogService.tweet("secureSignup onPost2")
            // TODO(mike): send email with capAndMac
            CompletionMapper.complete(key, compact(render(
              ("msgType" -> "createUserResponse") ~
              ("content" -> ("agentURI" -> ("agent://cap/" + capAndMac))) 
            )))
          }
        )
      }
    )
  }

  def createUserRequest(json : JValue, key : String): Unit = {
    import DSLCommLink.mTT
    val email = (json \ "content" \ "email").extract[String].toLowerCase

    if (email == "") {
      // No email, sign up immediately with a random cap
      secureSignup(
        "",
        (json \ "content" \ "password").extract[String],
        compact(render(json \ "content" \ "jsonBlob")), 
        key
      )
    } else {
      // Email provided; send a confirmation email
      import ConfirmationEmail._
      val token = UUID.randomUUID.toString.substring(0,8)
      val tokenUri = new URI("token://" + token)
      val tokenCnxn = PortableAgentCnxn(tokenUri, "token", tokenUri)

      val (erql, erspl) = agentMgr().makePolarizedPair()
      agentMgr().post[String](erql, erspl)(
        tokenFilter,
        List(tokenCnxn),
        // email, password, and jsonBlob
        compact(render(json \ "content")),
        (dummy: Option[mTT.Resource]) => {
          confirm(email, token)
          // Notify user to check her email
          CompletionMapper.complete(key, compact(render(
            ("msgType" -> "createUserWaiting") ~
            ("content" -> List()) // List() is rendered as "{}" 
          )))
        }
      )
    }
  }
  

  def secureLogin(
    identType: String,
    identInfo: String,
    password: String,
    key: String
  ) : Unit = {
    import DSLCommLink.mTT
    
    def login(cap: String): Unit = {
      val capURI = new URI("usercap://" + cap)
      val capSelfCnxn = PortableAgentCnxn(capURI, "pwdb", capURI)
      val onPwmacFetch: Option[mTT.Resource] => Unit = (rsrc) => {
        BasicLogService.tweet("secureLogin | login | onPwmacFetch: rsrc = " + rsrc)
        rsrc match {
          // At this point the cap is good, but we have to verify the pw mac
          case None => ()
          case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
            BasicLogService.tweet("secureLogin | login | onPwmacFetch: Cap is good")
            postedExpr.asInstanceOf[PostedExpr[String]] match {
              case PostedExpr(pwmac) => {
                BasicLogService.tweet ("secureLogin | login | onPwmacFetch: pwmac = " + pwmac)
                val macInstance = Mac.getInstance("HmacSHA256")
                macInstance.init(new SecretKeySpec("pAss#4$#".getBytes("utf-8"), "HmacSHA256"))
                val hex = macInstance.doFinal(password.getBytes("utf-8")).map("%02x" format _).mkString
                BasicLogService.tweet ("secureLogin | login | onPwmacFetch: hex = " + hex)
                if (hex != pwmac.toString) {
                  BasicLogService.tweet("secureLogin | login | onPwmacFetch: Password mismatch.")
                  CompletionMapper.complete(key, compact(render(
                    ("msgType" -> "initializeSessionError") ~
                    ("content" -> ("reason" -> "Bad password.")) 
                  )))
                } else {
                  val onUserDataFetch: Option[mTT.Resource] => Unit = (optRsrc) => {
                    BasicLogService.tweet("secureLogin | login | onPwmacFetch | onUserDataFetch: optRsrc = " + optRsrc)
                    optRsrc match {
                      case None => ()
                      case Some(rbnd@mTT.RBoundHM(Some(mTT.Ground(postedexpr)), _)) => {
                        // TODO(mike): fill in response with bindings
                        val bindings = rbnd.sbst.getOrElse(throw new Exception(""))
                        postedexpr.asInstanceOf[PostedExpr[String]] match {
                          case PostedExpr(jsonBlob) => {
                            val content = 
                              ("sessionURI" -> ("agent-session://" + cap)) ~
                              ("listOfAliases" -> List[String]()) ~
                              ("defaultAlias" -> "") ~
                              ("listOfLabels" -> List[String]()) ~
                              ("listOfCnxns" -> List[String]()) ~
                              ("lastActiveFilter" -> "") ~
                              ("jsonBlob" -> parse(jsonBlob))

                            CompletionMapper.complete(key, compact(render(
                              ("msgType" -> "initializeSessionResponse") ~
                              ("content" -> content)
                            )))
                          }
                        }
                      }
                      case _ => {
                        throw new Exception("Unrecognized resource: " + optRsrc)
                      }
                    }
                  }
                  val (erql, erspl) = agentMgr().makePolarizedPair()
                  agentMgr().fetch( erql, erspl )(userDataFilter, List(capSelfCnxn), onUserDataFetch)
                  ()
                }
              }
              case _ => BasicLogService.tweet("PostedExpr problem: " + postedExpr)
            }
          }
          case _ => {
            BasicLogService.tweet("Unrecognized resource: " + rsrc)
          }
        }
      }
      val (erql, erspl) = agentMgr().makePolarizedPair()
      BasicLogService.tweet ("secureLogin | login: fetching with eqrl, erspl = " + erql + ", " + erspl)
      agentMgr().fetch( erql, erspl )(pwmacFilter, List(capSelfCnxn), onPwmacFetch)
    }
    
    // identType is either "cap" or "email"
    identType match {
      case "cap" => {
        BasicLogService.tweet("secureLogin | cap branch")
        val cap = identInfo.slice(0, 36)
        val mac = identInfo.slice(36, 46)
        val macInstance = Mac.getInstance("HmacSHA256")
        macInstance.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
        val hex = macInstance.doFinal(cap.getBytes("utf-8")).slice(0,5).map("%02x" format _).mkString
        if (hex != mac) {
          CompletionMapper.complete(key, compact(render(
            ("msgType" -> "initializeSessionError") ~
            ("content" -> ("reason" -> "This link wasn't generated by us.")) 
          )))
        } else {
          BasicLogService.tweet("Link OK, logging in")
          login(cap)
        }
      }
      
      case "email" => {
        val lcemail = identInfo.toLowerCase
        BasicLogService.tweet("secureLogin | email branch: lcemail = " + lcemail)
        // hash the email to get cap
        val md = MessageDigest.getInstance("SHA1")
        md.update(lcemail.getBytes("utf-8"))
        val cap = md.digest().map("%02x" format _).mkString.substring(0,36)
        // don't need mac of cap; need to verify email is on our network
        val emailURI = new URI("emailhash://" + cap)
        val emailSelfCnxn = PortableAgentCnxn(emailURI, "emailhash", emailURI)
        val (erql, erspl) = agentMgr().makePolarizedPair()
        BasicLogService.tweet("secureSignup | email branch: erql, erspl = " + erql + ", " + erspl)
        agentMgr().fetch(erql, erspl)(
          emailFilter,
          List(emailSelfCnxn),
          (optRsrc: Option[mTT.Resource]) => {
            BasicLogService.tweet("secureLogin | email case | anonymous onFetch: optRsrc = " + optRsrc)
            optRsrc match {
              case None => ()
              case Some(mTT.RBoundHM(Some(mTT.Ground(postedexpr)), _)) => {
                postedexpr.asInstanceOf[PostedExpr[String]] match {
                  case PostedExpr(cap) => {
                    login(cap)
                  }
                }
              }
              case _ => {
                throw new Exception("Unrecognized resource: optRsrc = " + optRsrc)
              }
            }
          }
        )
      }
    }
  }

  def initializeSessionRequest(
    json : JValue,
    key : String,
    onPost : Option[Option[mTT.Resource] => Unit] = None
  ): Unit = {
    val agentURI = (json \ "content" \ "agentURI").extract[String]
    val uri = new URI(agentURI)

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
    secureLogin(identType, identInfo, password, key)
  }

  def evalSubscribeRequest(json: JValue, cometMessageJSON: (String, String) => Unit) : Unit = {
    import com.biosimilarity.evaluator.distribution.portable.v0_1._

    BasicLogService.tweet("evalSubscribeRequest: json = " + compact(render(json)));
    val content = (json \ "content").asInstanceOf[JObject]
    val sessionURIstr = (content \ "sessionURI").extract[String]
    val (erql, erspl) = agentMgr().makePolarizedPair()
    
    val expression = (content \ "expression")
    val exprType = (expression \ "msgType").extract[String]
    case class CX(src: String, label: String, tgt: String)
    case class E(filter: String, cnxns: List[CX])
    case class IC(filter: String, cnxns: List[CX], value: String)
    exprType match {
      case "feedExpr" => {
        BasicLogService.tweet("evalSubscribeRequest | feedExpr")
        val onFeed: Option[mTT.Resource] => Unit = (rsrc) => {
          BasicLogService.tweet("evalSubscribeRequest | onFeed: rsrc = " + rsrc)
          rsrc match {
            case None => ()
            case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
              postedExpr.asInstanceOf[PostedExpr[String]] match {
                case PostedExpr(postedStr) => {
                  val content =
                    ("sessionURI" -> sessionURIstr) ~
                    ("pageOfPosts" -> List(postedStr))
                  val response = ("msgType" -> "evalSubscribeResponse") ~ ("content" -> content)
                  BasicLogService.tweet("evalSubscribeRequest | onFeed: response = " + compact(render(response)))
                  cometMessageJSON(sessionURIstr, compact(render(response)))
                }
              }
            }
            case _ => throw new Exception("Unrecognized resource: " + rsrc)
          }
        }
        val fe = (expression \ "content").extract[E]
        val filter = fromTermString(fe.filter).getOrElse(
          throw new Exception("Couldn't parse filter: " + compact(render(json)))
        )
        val cnxns = fe.cnxns.map((cx: CX) => new PortableAgentCnxn(new URI(cx.src), cx.label, new URI(cx.tgt)))
        BasicLogService.tweet("evalSubscribeRequest | feedExpr: calling feed")
        agentMgr().feed(erql, erspl)(filter, cnxns, onFeed)
      }
      case "scoreExpr" => {
        BasicLogService.tweet("evalSubscribeRequest | scoreExpr")
        val onScore: Option[mTT.Resource] => Unit = (rsrc) => {
          BasicLogService.tweet("evalSubscribeRequest | onScore: rsrc = " + rsrc)
          rsrc match {
            case None => ()
            case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
              postedExpr.asInstanceOf[PostedExpr[String]] match {
                case PostedExpr(postedStr) => {
                  val content =
                    ("sessionURI" -> sessionURIstr) ~
                    ("pageOfPosts" -> List(postedStr))
                  val response = ("msgType" -> "evalSubscribeResponse") ~ ("content" -> content)
                  BasicLogService.tweet("evalSubscribeRequest | onScore: response = " + compact(render(response)))
                  cometMessageJSON(sessionURIstr, compact(render(response)))
                }
              }
            }
            case _ => throw new Exception("Unrecognized resource: " + rsrc)
          }
        }
        val se = (expression \ "content").extract[E]
        val filter = fromTermString(se.filter).getOrElse(
          throw new Exception("Couldn't parse filter: " + compact(render(json)))
        )
        val cnxns = se.cnxns.map((cx: CX) => 
          new PortableAgentCnxn(new URI(cx.src), cx.label, new URI(cx.tgt))
        )
        val staff = (expression \ "content" \ "staff") match {
          case JObject(List((which: String, vals@JArray(_)))) => {
            // Either[Seq[PortableAgentCnxn],Seq[CnxnCtxtLabel[String,String,String]]]
            which match {
              case "a" => Left(
                vals.extract[List[CX]].map((cx: CX) => 
                  new PortableAgentCnxn(new URI(cx.src), cx.label, new URI(cx.tgt))
                )
              )
              case "b" => Right(
                vals.extract[List[String]].map((t: String) => 
                  fromTermString(t).getOrElse(throw new Exception("Couldn't parse staff: " + json))
                )
              )
              case _ => throw new Exception("Couldn't parse staff: " + json)
            }
          }
          case _ => throw new Exception("Couldn't parse staff: " + json)
        }
        BasicLogService.tweet("evalSubscribeRequest | feedExpr: calling score")
        agentMgr().score(erql, erspl)(filter, cnxns, staff, onScore)
      }
      case "insertContent" => {
        BasicLogService.tweet("evalSubscribeRequest | insertContent")
        val onPost: Option[mTT.Resource] => Unit = (rsrc) => {
          BasicLogService.tweet("evalSubscribeRequest | onPost: rsrc = " + rsrc)
          // evalComplete, empty seq of posts
          val content =
            ("sessionURI" -> sessionURIstr) ~
            ("pageOfPosts" -> List[String]())
          val response = ("msgType" -> "evalComplete") ~ ("content" -> content)
          BasicLogService.tweet("evalSubscribeRequest | onPost: response = " + compact(render(response)))
          cometMessageJSON(sessionURIstr, compact(render(response)))
        }
        try {
          BasicLogService.tweet("evalSubscribeRequest | insertContent | before ic")
          val ic = (expression \ "content").extract[IC]
          BasicLogService.tweet("evalSubscribeRequest | insertContent | before filter")
          val filter = fromTermString(ic.filter).getOrElse(
            throw new Exception("Couldn't parse filter: " + compact(render(json)))
          )
          BasicLogService.tweet("evalSubscribeRequest | insertContent | before cnxns")
          val cnxns = ic.cnxns.map((cx: CX) => 
            new PortableAgentCnxn(new URI(cx.src), cx.label, new URI(cx.tgt))
          )
          BasicLogService.tweet("evalSubscribeRequest | insertContent: calling post")
          agentMgr().post(erql, erspl)(filter, cnxns, ic.value, onPost)
        } catch {
          case (ex: Exception) => {
            BasicLogService.tweet("evalSubscribeRequest | insertContent | exception: ex = " + ex.getStackTrace.mkString("\n"))
            throw ex
          }
        }
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
