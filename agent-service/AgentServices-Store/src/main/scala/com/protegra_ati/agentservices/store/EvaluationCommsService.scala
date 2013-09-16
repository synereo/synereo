// -*- mode: Scala;-*- 
// Filename:    EvaluationCommsService.scala 
// Authors:     lgm                                                    
// Creation:    Thu May  2 21:31:06 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.protegra_ati.agentservices.store._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.concurrent.duration._
import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import com.typesafe.config._

import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import java.security._

import java.net.URI
import java.util.Date
import java.util.UUID



trait EvaluationCommsService extends CnxnString[String, String, String]{
  self : EvalConfig with DSLCommLinkConfiguration =>

  import DSLCommLink._   
  import Being._
  import PersistedKVDBNodeFactory._
  import DSLCommLinkCtor._
  import diesel.ConcreteHumanEngagement._
  import ConcreteHL._
  
  var _clientServerPair : Option[
    (
      Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse],
      Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
    )
  ] = None

  var _node : Option[
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  ] = None 

  def link() : (
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse],
    Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
  ) = {
    _clientServerPair match {
      case Some( lnk ) => lnk
      case None => {        
        val ( client, server ) : ( Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ], Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] ) = DSLCommLinkCtor.stdBiLink(
          clientHostName, clientPort,
          serverHostName, serverPort
        )

        _clientServerPair = Some( ( client, server ) )
        ( client, server )
      }
    }    
  }

  def node(
    flip : Boolean = false
  ) : Being.PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse] = {
    _node match {
      case Some( n ) => n
      case None => {        
        val n : Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] = DSLCommLinkCtor.stdLink(
          serverHostName, serverPort,
          clientHostName, clientPort
        )( flip )

        _node = Some( n )
        n
      }
    }    
  }

  trait AgentManager extends Journalist {
    def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def makePolarizedPair() = {
      val sessionID = UUID.randomUUID
      (erql( sessionID ), erspl( sessionID ))
    }

    def adminErql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def adminErspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def makePolarizedAdminPair() = {
      val sessionID = UUID.randomUUID
      (adminErql( sessionID ), adminErspl( sessionID ))
    }

    val userDataFilter = fromTermString(
        //"userData(listOfAliases(A), defaultAlias(DA), listOfLabels(L), listOfCnxns(C), lastActiveFilter(F))"
        "userData(X)"
      ).getOrElse(throw new Exception("Couldn't parse userDataFilter"))
    val pwmacFilter = fromTermString("pwmac(X)").getOrElse(throw new Exception("Couldn't parse pwmacFilter"))
    val emailFilter = fromTermString("email(X)").getOrElse(throw new Exception("Couldn't parse emailFilter"))

    // Under what conditions can this fail?
    def secureSignup(
      email: String,
      password: String,
      jsonBlob: String,
      complete: Either[String, String] => Unit
    ) : Unit = {
      import DSLCommLink.mTT
      val lcemail = email.toLowerCase
      val cap = if (lcemail == "") UUID.randomUUID.toString else {
        // If email is nonempty, hash it for the cap part
        val md = MessageDigest.getInstance("SHA1")
        md.update(lcemail.getBytes("utf-8"))
        val cap = md.digest().map("%02x" format _).mkString.substring(0,36)
        val emailURI = new URI("emailhash://" + cap)
        val emailSelfCnxn = //new ConcreteHL.PortableAgentCnxn(emailURI, emailURI.toString, emailURI)
          PortableAgentCnxn(emailURI, "emailhash", emailURI)
        // TODO(mike): validate structure of email address
        // TODO(mike): delay this until after we've received confirmation that
        //   the owner of this email address wants to sign up.
        //   Signing up twice is safe, since the cap is a pure function of the email.
        // TODO(mike): use a mac instead of a hash
        val (erql, erspl) = makePolarizedPair()
        post[String](erql, erspl)(
          emailFilter,
          List(emailSelfCnxn),
          cap
        )
        cap
      }
      tweet("secureSignup lcemail="+lcemail+", password="+password+", cap="+cap)
      val macInstance = Mac.getInstance("HmacSHA256")
      macInstance.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
      val mac = macInstance.doFinal(cap.getBytes("utf-8")).slice(0,5).map("%02x" format _).mkString
      val capAndMac = cap + mac
      val capURI = new URI("usercap://" + cap)
      val capSelfCnxn = PortableAgentCnxn(capURI, "pwdb", capURI)

      macInstance.init(new SecretKeySpec("pAss#4$#".getBytes("utf-8"), "HmacSHA256"))
      val pwmac = macInstance.doFinal(password.getBytes("utf-8")).map("%02x" format _).mkString

      val onPost: Option[mTT.Resource] => Unit = ( dummy : Option[mTT.Resource] ) => {
        tweet("secureSignup onPost1")
        // Change String to Term throughout.
        val (erql, erspl) = makePolarizedPair()
        post[String](erql, erspl)(
          userDataFilter,
          List(capSelfCnxn),
          // "userData(listOfAliases(), defaultAlias(\"\"), listOfLabels(), " +
          //     "listOfCnxns(), lastActiveFilter(\"\"))",
          jsonBlob,
          ( dummy : Option[mTT.Resource] ) => {
            tweet("secureSignup onPost2")
            // TODO(mike): send email with capAndMac
            complete(Left(capAndMac))
          }
        )
      }
      tweet("secureSignup posting pwmac")
      val (erql, erspl) = makePolarizedPair()
      post[String](erql, erspl)(
        pwmacFilter,
        List(capSelfCnxn),
        pwmac,
        onPost
      )
    }
    
    def secureLogin(
      identType: String,
      identInfo: String,
      password: String,
      complete: String => Unit
    ) : Unit = {
      import DSLCommLink.mTT
      
      def login(cap: String): Unit = {
        val capURI = new URI("usercap://" + cap)
        val capSelfCnxn = PortableAgentCnxn(capURI, "pwdb", capURI)
        val onPwmacFetch: Option[mTT.Resource] => Unit = (rsrc) => {
          tweet("secureLogin | login | onPwmacFetch: rsrc = " + rsrc)
          rsrc match {
            // At this point the cap is good, but we have to verify the pw mac
            case None => ()
            case Some(mTT.RBoundHM(Some(mTT.Ground(postedExpr)), _)) => {
              tweet("secureLogin | login | onPwmacFetch: Cap is good")
              postedExpr.asInstanceOf[PostedExpr[String]] match {
                case PostedExpr(pwmac) => {
                  tweet ("secureLogin | login | onPwmacFetch: pwmac = " + pwmac)
                  val macInstance = Mac.getInstance("HmacSHA256")
                  macInstance.init(new SecretKeySpec("pAss#4$#".getBytes("utf-8"), "HmacSHA256"))
                  val hex = macInstance.doFinal(password.getBytes("utf-8")).map("%02x" format _).mkString
                  tweet ("secureLogin | login | onPwmacFetch: hex = " + hex)
                  if (hex != pwmac.toString) {
                    tweet("secureLogin | login | onPwmacFetch: Password mismatch.")
                    complete("Bad password.")
                  } else {
                    val onUserDataFetch: Option[mTT.Resource] => Unit = (optRsrc) => {
                      tweet("secureLogin | login | onPwmacFetch | onUserDataFetch: optRsrc = " + optRsrc)
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

                              complete(compact(render(
                                ("msgType" -> "initializeSessionResponse") ~
                                ("content" -> content)
                              )))
                            }
                          }
                        }
                      }
                    }
                    val (erql, erspl) = makePolarizedPair()
                    fetch( erql, erspl )(userDataFilter, List(capSelfCnxn), onUserDataFetch)
                    ()
                  }
                }
                case _ => tweet("PostedExpr problem.")
              }
            }
            case _ => {
              tweet("Unrecognized resource")
            }
          }
        }
        val (erql, erspl) = makePolarizedPair()
        tweet ("secureLogin | login: fetching with eqrl, erspl = " + erql + ", " + erspl)
        fetch( erql, erspl )(pwmacFilter, List(capSelfCnxn), onPwmacFetch)
      }
      
      // identType is either "cap" or "email"
      identType match {
        case "cap" => {
          tweet("secureLogin | cap branch")
          val cap = identInfo.slice(0, 36)
          val mac = identInfo.slice(36, 46)
          val macInstance = Mac.getInstance("HmacSHA256")
          macInstance.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
          val hex = macInstance.doFinal(cap.getBytes("utf-8")).slice(0,5).map("%02x" format _).mkString
          if (hex != mac) {
            complete("This link wasn't generated by us.")
          } else {
            tweet("Link OK, logging in")
            login(cap)
          }
        }
        
        case "email" => {
          val lcemail = identInfo.toLowerCase
          tweet("secureLogin | email branch: lcemail = " + lcemail)
          // hash the email to get cap
          val md = MessageDigest.getInstance("SHA1")
          md.update(lcemail.getBytes("utf-8"))
          val cap = md.digest().map("%02x" format _).mkString.substring(0,36)
          // don't need mac of cap; need to verify email is on our network
          val emailURI = new URI("emailhash://" + cap)
          val emailSelfCnxn = PortableAgentCnxn(emailURI, "emailhash", emailURI)
          val (erql, erspl) = makePolarizedPair()
          tweet("secureSignup | email branch: erql, erspl = " + erql + ", " + erspl)
          fetch(erql, erspl)(
            emailFilter,
            List(emailSelfCnxn),
            (optRsrc: Option[mTT.Resource]) => {
              tweet("secureLogin | email case | anonymous onFetch: optRsrc = " + optRsrc)
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
    def post[Value](
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      content : Value,
      onPost : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, InsertContent( filter, cnxns, content ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onPost( e ) }
      }
    }
    // TODO(metaweta): factor case class out of read, fetch, and feed
    def read(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onReadRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, ReadExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onReadRslt( e ) }
      }
    }
    def fetch(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFetchRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, FetchExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onFetchRslt( e ) }
      }
    }
    def feed(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      onFeedRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, FeedExpr( filter, cnxns ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onFeedRslt( e ) }
      }
    }
    def score(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      cnxns : Seq[Cnxn],
      staff : Either[Seq[Cnxn],Seq[Label]],
      onScoreRslt : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { tweet( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, ScoreExpr( filter, cnxns, staff ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onScoreRslt( e ) }
      }
    }
  }

  def ensureServersConnected(
    pulseErql : CnxnCtxtLabel[String,String,String],
    pulseErspl : CnxnCtxtLabel[String,String,String]
  )(
    onConnection : Option[mTT.Resource] => Unit =
      ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
  ) : Unit = {
    // post to channel
    reset {
      node().put( pulseErql, ConcreteHL.Bottom )
    }
    
    // wait for response
    reset {
      for( e <- node().get( pulseErspl ) ) {
        onConnection( e )
      }
    }
  }

  @transient
  var _agentMgr : Option[AgentManager with Serializable] = None
  def agentMgr( flip : Boolean = false ) : AgentManager with Serializable = {
    _agentMgr match {
      case Some( agntMgr ) => agntMgr
      case None => {    
        node( flip )
        val agntMgr =
          new AgentManager with Serializable {
            override def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalRequestLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make evalRequestLabel" ) )
            }
            override def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalResponseLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make evalResponseLabel" ) )
            }
            override def adminErql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.adminRequestLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make adminRequestLabel" ) )
            }
            override def adminErspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.adminResponseLabel()(
                Left[String,String]( sessionID.toString )
                //Right[String,String]( "SessionId" )
              ).getOrElse( throw new Exception( "unable to make adminResponseLabel" ) )
            }
          }

        _agentMgr = Some( agntMgr )

        agntMgr
      }
    }    
  }
}

package usage {
  object EvaluationServiceContext
  extends Serializable 
  with UseCaseHelper {    
    @transient
    lazy val eServe =
      new EvaluationCommsService
         with EvalConfig
         with DSLCommLinkConfiguration
         with Serializable {
         }    
  }  
}
