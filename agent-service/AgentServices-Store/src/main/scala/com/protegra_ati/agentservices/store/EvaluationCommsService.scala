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

  trait AgentManager {
    def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def adminErql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def adminErspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    val userDataFilter = fromTermString(
        "userData(listOfAliases(A), defaultAlias(DA), listOfLabels(L), listOfCnxns(C), lastActiveFilter(F))"
        //"userData(X)"
      ).getOrElse(throw new Exception("Couldn't parse userDataFilter"))
    val pwmacFilter = fromTermString("pwmac(X)").getOrElse(throw new Exception("Couldn't parse pwmacFilter"))
    val emailFilter = fromTermString("email(X)").getOrElse(throw new Exception("Couldn't parse emailFilter"))

    // Under what conditions can this fail?
    def secureSignup(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      email: String,
      password: String,
      complete: Either[String, String] => Unit
    ) : Unit = {
      import DSLCommLink.mTT
      val lcemail = email.toLowerCase
      val cap = if (lcemail == "") UUID.randomUUID.toString else {
        // If email is nonempty, hash it for the cap part
        val md = MessageDigest.getInstance("SHA-256")
        md.update(lcemail.getBytes("utf-8"))
        val cap = md.digest().map("%02x" format _).mkString.substring(0,36)
        val emailURI = new URI("mailto://" + lcemail)
        val emailSelfCnxn = //new ConcreteHL.PortableAgentCnxn(emailURI, emailURI.toString, emailURI)
          PortableAgentCnxn(emailURI, emailURI.toString, emailURI)
        // TODO(mike): validate structure of email address
        // TODO(mike): delay this until after we've received confirmation that
        //   the owner of this email address wants to sign up.
        //   Signing up twice is safe, since the cap is a pure function of the email.
        post[String](erql, erspl)(
          emailFilter,
          List(emailSelfCnxn),
          cap
        )
        cap
      }
      println("secureSignup lcemail="+lcemail+", password="+password+", cap="+cap)
      val macInstance = Mac.getInstance("HmacSHA256")
      macInstance.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
      val mac = macInstance.doFinal(cap.getBytes("utf-8")).slice(0,5).map("%02x" format _).mkString
      val capAndMac = cap + mac
      val capURI = new URI("usercap://" + cap)
      val capSelfCnxn = //new ConcreteHL.PortableAgentCnxn(capURI, "pwdb", capURI)
        PortableAgentCnxn(capURI, "pwdb", capURI)

      macInstance.init(new SecretKeySpec("pAss#4$#".getBytes("utf-8"), "HmacSHA256"))
      val pwmac = macInstance.doFinal(password.getBytes("utf-8")).map("%02x" format _).mkString

      val onPost: Option[mTT.Resource] => Unit = ( dummy : Option[mTT.Resource] ) => {
        println("secureSignup onPost1")
        post[String](erql, erspl)(
          userDataFilter,
          List(capSelfCnxn),
          "userData(listOfAliases(), defaultAlias(\"\"), listOfLabels(), " +
              "listOfCnxns(), lastActiveFilter(\"\"))",
          ( dummy : Option[mTT.Resource] ) => {
            println("secureSignup onPost2")
            // TODO(mike): send email with capAndMac
            complete(Left(capAndMac))
          }
        )
      }
      println("secureSignup posting pwmac")
      post[String](erql, erspl)(
        pwmacFilter,
        List(capSelfCnxn),
        pwmac,
        onPost
      )
    }
    
    def secureLogin( 
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      identType: String,
      identInfo: String,
      password: String,
      complete: String => Unit
    ) : Unit = {
      import DSLCommLink.mTT
      
      def login(cap: String): Unit = {
        val capURI = new URI("usercap://" + cap)
        val capSelfCnxn = //new ConcreteHL.PortableAgentCnxn(capURI, "pwdb", capURI)
          PortableAgentCnxn(capURI, "pwdb", capURI)
        val onPwmacFetch: Option[mTT.Resource] => Unit = (rsrc) => {
          println("secureLogin | login | onPwmacFetch: rsrc = " + rsrc)
          rsrc match {
            // At this point the cap is good, but we have to verify the pw mac
            case None => ()
            case Some(mTT.RBoundHM(Some(mTT.Ground(postedexpr)), _)) => {
              println("secureLogin | login | onPwmacFetch: Cap is good")
              postedexpr.asInstanceOf[PostedExpr[String]] match {
                case PostedExpr(pwmac) => {
                  println ("secureLogin | login | onPwmacFetch: pwmac = " + pwmac)
                  val macInstance = Mac.getInstance("HmacSHA256")
                  macInstance.init(new SecretKeySpec("pAss#4$#".getBytes("utf-8"), "HmacSHA256"))
                  val hex = macInstance.doFinal(password.getBytes("utf-8")).map("%02x" format _).mkString
                  println ("secureLogin | login | onPwmacFetch: hex = " + hex)
                  if (hex != pwmac.toString) {
                    println("secureLogin | login | onPwmacFetch: Password mismatch.")
                    complete("Bad password.")
                  } else {
                    val onUserDataFetch: Option[mTT.Resource] => Unit = (optRsrc) => {
                      println("secureLogin | login | onPwmacFetch | onUserDataFetch: optRsrc = " + optRsrc)
                      optRsrc match {
                        case None => ()
                        case Some(rbnd: mTT.RBound) => {
                          // TODO(mike): fill in response with bindings
                          val bindings = rbnd.sbst.getOrElse(throw new Exception(""))
                          complete(
                            """{
                              "msgType": "initializeSessionResponse",
                              "content": {
                                "sessionURI": "agent-session://ArtVandelay@session1",
                                "listOfAliases": [],
                                "defaultAlias": "",
                                "listOfLabels": [],
                                "listOfCnxns": [],
                                "lastActiveFilter": ""
                              }
                            }
                            """
                          )
                        }
                      }
                    }
                    fetch( erql, erspl )(userDataFilter, List(capSelfCnxn), onUserDataFetch)
                    ()
                  }
                }
                case _ => println("PostedExpr problem.")
              }
            }
            case _ => {
              println("Unrecognized resource")
            }
          }
        }
        fetch( erql, erspl )(pwmacFilter, List(capSelfCnxn), onPwmacFetch)
      }
      
      // identType is either "cap" or "email"
      identType match {
        case "cap" => {
          val cap = identInfo.slice(0, 36)
          val mac = identInfo.slice(36, 46)
          val macInstance = Mac.getInstance("HmacSHA256")
          macInstance.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
          val hex = macInstance.doFinal(cap.getBytes("utf-8")).slice(0,5).map("%02x" format _).mkString
          if (hex != mac) {
            complete("This link wasn't generated by us.")
          } else {
            login(cap)
          }
        }
        
        case "email" => {
          val email = identInfo.toLowerCase
          // hash the email to get cap
          val md = MessageDigest.getInstance("SHA256")
          val cap = md.digest(email.getBytes("UTF-8")).
              map("%02x" format _).mkString.slice(0,36)
          // don't need mac; need to verify email is on our network
          val emailURI = new URI("mailto://" + email)
          val emailSelfCnxn = //new ConcreteHL.PortableAgentCnxn(emailURI, emailURI.toString, emailURI)
            PortableAgentCnxn(emailURI, emailURI.toString, emailURI)
          fetch(erql, erspl)(
            emailFilter,
            List(emailSelfCnxn),
            (optRsrc: Option[mTT.Resource]) => {
              optRsrc match {
                case Some(mTT.Ground(cap: ConcreteHL.HLExpr)) => {
                  login(cap.toString)
                }
              }
            }
          )
        }
      }
      
    }    
    def createAgent(
      erql : CnxnCtxtLabel[String,String,String],
      erspl : CnxnCtxtLabel[String,String,String]
    )(
      filter : CnxnCtxtLabel[String,String,String],
      selfCnxn : Cnxn,
      thisUser : User[String,String,String],
      onCreation : Option[mTT.Resource] => Unit =
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
    ) : Unit = {
      reset {
        node().publish( erql, InsertContent( filter, List( selfCnxn ), thisUser ) )
      }
      reset {
        for( e <- node().subscribe( erspl ) ) { onCreation( e ) }
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
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
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
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
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
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
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
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
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
        ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
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
