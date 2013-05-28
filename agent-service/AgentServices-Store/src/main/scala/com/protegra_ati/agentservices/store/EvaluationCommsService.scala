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

import java.net.URI
import java.util.Date
import java.util.UUID



trait EvaluationCommsService extends CnxnString[String, String, String]{
  self : EvalConfig =>

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
        val dslCommLinkHost =
          try {
            evalConfig.getString( "DSLCommLinkHost" )
          }
          catch {
            case e : Throwable => "10.0.1.10"
          }
        val dslCommLinkPort = 
          try {
            evalConfig.getInt( "DSLCommLinkPort" )
          }
          catch {
            case e : Throwable => 5672
          }
        val dslCommLinkRemoteHost = 
          try {
            evalConfig.getString( "DSLCommLinkRemoteHost" )
          }
          catch {
            case e : Throwable => "10.0.1.8"
          }
        val dslCommLinkRemotePort = 
          try {
            evalConfig.getInt( "DSLCommLinkRemotePort" )
          }
          catch {
            case e : Throwable => 5672
          }

        val ( client, server ) : ( Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ], Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] ) = DSLCommLinkCtor.stdBiLink(
          dslCommLinkHost, dslCommLinkPort,
          dslCommLinkRemoteHost, dslCommLinkRemotePort
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
        val dslCommLinkHost =
          try {
            evalConfig.getString( "DSLCommLinkHost" )
          }
          catch {
            case e : Throwable => "localhost"
          }
        val dslCommLinkPort = 
          try {
            evalConfig.getInt( "DSLCommLinkPort" )
          }
          catch {
            case e : Throwable => 5672
          }
        val dslCommLinkRemoteHost = 
          try {
            evalConfig.getString( "DSLCommLinkRemoteHost" )
          }
          catch {
            case e : Throwable => "localhost"
          }
        val dslCommLinkRemotePort = 
          try {
            evalConfig.getInt( "DSLCommLinkRemotePort" )
          }
          catch {
            case e : Throwable => 5672
          }

        val n : Being.PersistedMonadicKVDBNode[
          PersistedKVDBNodeRequest,PersistedKVDBNodeResponse
        ] = DSLCommLinkCtor.stdLink(
          dslCommLinkHost, dslCommLinkPort,
          dslCommLinkRemoteHost, dslCommLinkRemotePort
        )( flip )

        _node = Some( n )
        n
      }
    }    
  }

  trait AgentManager {
    def erql( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String]
    def secureCnxn( 
        userName: String, 
        userPwd: String, 
        queryMap: HashMap[String, String], 
        post: (CnxnCtxtLabel[String,String,String], Seq[Cnxn], String, Option[mTT.Resource] => Unit) => Unit,
        finalOnPost: Option[mTT.Resource] => Unit
    ) = {
      // TODO: move all keys out of the server code

      // TODO: use interpolation everywhere below
      // http://docs.scala-lang.org/overviews/core/string-interpolation.html
      // fromTermString(prolog"~")

      // Hash public user data to get a convenient key for the password database
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(new SecretKeySpec("5ePeN42X".getBytes("utf-8"), "HmacSHA256"))
      val hex1 = mac.doFinal(userName.getBytes("utf-8")).map("%02x" format _).mkString
      val pwdbURI = new URI("pwdb://" + hex1)
      // Create a self connection to store this entry's data
      val pwdbCnxn = new ConcreteHL.PortableAgentCnxn(pwdbURI, "pwdb", pwdbURI)

      // Hash actual password to test given password against in the future
      mac.init(new SecretKeySpec("X@*h$ikU".getBytes("utf-8"), "HmacSHA256"))
      val pwHashStr = mac.doFinal(userPwd.getBytes("utf-8")).map("%02x" format _).mkString
      val hashTermStr = "hash(\"" + pwHashStr + "\")"
      val hashTerm = fromTermString(hashTermStr).getOrElse(
          throw new Exception("hashTermStr failed to parse: " + hashTermStr)
        )
      
      val onPost = ( optRsrc : Option[mTT.Resource] ) => { println( "got response: " + optRsrc ) }
      // Store password hash in database
      post(hashTerm, List(pwdbCnxn), "", onPost)

      // The special root of all this user's authority
      val userCap = UUID.randomUUID

      // Prepare to store the root cap securely using user's password
      mac.init(new SecretKeySpec("8Uh4Fzs9".getBytes("utf-8"), "HmacSHA256"))
      val capEncryptKey = mac.doFinal(userPwd.getBytes("utf-8")).slice(0, 16)
      val capEncryptSpec = new SecretKeySpec(capEncryptKey, "AES")
      val encrypt = Cipher.getInstance("AES")
      encrypt.init(Cipher.ENCRYPT_MODE, capEncryptSpec)
      val userEncryptedCap = encrypt.doFinal(userCap.toString.getBytes("utf-8")).map("%02x" format _).mkString
      val userEncryptedCapTerm = fromTermString("userEncryptedCap(\"" + userEncryptedCap + "\")").getOrElse(
          throw new Exception("userEncryptedCap failed to parse: " + userEncryptedCap)
        )
      // Store capability encrypted with user password in database
      post(userEncryptedCapTerm, List(pwdbCnxn), "", onPost)

      // Prepare to store the root cap securely using system password
      val sysCapEncryptSpec = new SecretKeySpec("BiosimilarityLLC".getBytes("utf-8"), "AES")
      encrypt.init(Cipher.ENCRYPT_MODE, sysCapEncryptSpec)
      val sysEncryptedCap = encrypt.doFinal(userCap.toString.getBytes("utf-8")).map("%02x" format _).mkString
      val sysEncryptedCapTerm = fromTermString("sysEncryptedCap(\"" + sysEncryptedCap + "\")").getOrElse(
          throw new Exception("sysEncryptedCap failed to parse: " + sysEncryptedCap)
        )
      // Store capability encrypted with system password in database
      post(sysEncryptedCapTerm, List(pwdbCnxn), "", onPost)
      
      // Prepare to store user's data using the root cap
      val userCapURI = new URI("usercap://" + userCap)
      val userSelfCnxn = new ConcreteHL.PortableAgentCnxn(userCapURI, userName, userCapURI)
      val userTermStr = "user(\"" + queryMap("fullname") +"\", \"" + queryMap("email") + "\")"
      val userTerm = fromTermString(userTermStr).getOrElse(
          throw new Exception("userTermStr failed to parse: " + userTermStr)
        )
      // Post user data to the user's self connection
      post(userTerm, List(userSelfCnxn), "", onPost)
      // Store username in public list of users
      // (When asking for list of connections, iterate over this public list, but drop yourself.
      // Form the connection by taking src, tgt to be of the form cnxn://<userName>
      // and the label to be "public" or something)
      val userdbURI = new URI("userdb:///");
      val userdbCnxn = new ConcreteHL.PortableAgentCnxn(userdbURI, "userdb", userdbURI);
      val userdbTermStr = "user(\"" + userName + "\")"
      val userdbTerm = fromTermString(userdbTermStr).getOrElse(
          throw new Exception("userdbTermStr failed to parse: " + userdbTermStr)
        )
      post(userdbTerm, List(userdbCnxn), "", finalOnPost)
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
                sessionID.toString
              ).getOrElse( throw new Exception( "unable to make evalRequestLabel" ) )
            }
            override def erspl( sessionID : UUID ) : CnxnCtxtLabel[String,String,String] = {
              ExchangeLabels.evalResponseLabel()(
                sessionID.toString
              ).getOrElse( throw new Exception( "unable to make evalResponseLabel" ) )
            }
          }

        _agentMgr = Some( agntMgr )

        agntMgr
      }
    }    
  }
}
