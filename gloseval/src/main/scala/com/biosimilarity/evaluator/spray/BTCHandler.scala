// -*- mode: Scala;-*- 
// Filename:    BTCHandler.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr 10 15:49:40 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution.portable.btc.v0_1._

import com.protegra_ati.agentservices.store._
import com.protegra_ati.agentservices.protocols.msgs._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.evaluator.msgs.agent.crud._
import com.biosimilarity.evaluator.prolog.PrologDSL._
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
import scala.collection.mutable.MapProxy
import scala.collection.mutable.HashMap

import com.typesafe.config._

import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import java.security._


import java.util.Date
import java.util.UUID

import java.net.URI
import java.net.URL

trait BTCPaymentStatus
case object BTCPaymentPending extends BTCPaymentStatus
case object BTCPaymentComplete extends BTCPaymentStatus
case object BTCPaymentFailed extends BTCPaymentStatus

object BTCPaymentSessions extends MapProxy[String,BTCPaymentStatus] with Serializable {
  @transient
  override val self = new HashMap[String,BTCPaymentStatus]()
}

trait BTCHandlerSchema {
  self : EvaluationCommsService with DownStreamHttpCommsT =>
 
  import DSLCommLink.mTT
  import ConcreteHL._
}

trait BTCHandler extends BTCHandlerSchema {
  self : EvaluationCommsService with DownStreamHttpCommsT =>
 
  import DSLCommLink.mTT
  import ConcreteHL._
  import BlockChainAPI._

  def btcReceivePaymentCallbackURL() : URL
  
  def handleSupportRequest(
    msg : supportRequest
  ) : Unit = {
    // set up a handler for the callback on payment receipt
    BTCPaymentSessions += ( msg.sessionId -> BTCPaymentPending )

    val btcWalletQry =
      fromTermString( s"""btc( walletAddress( Address ) )""" ).get    

    def handleRsp( v : ConcreteHL.HLExpr ) : Unit = {
      v match {
        case Bottom => {
          println(
            (
              "*********************************************************************************"
              + "\nwaiting for btc json data"
              + "\nmsg.to: " + msg.to
              + "\nbtcWalletQry: " + btcWalletQry
              + "\n*********************************************************************************"
            )
          )
          BasicLogService.tweet(
            (
              "*********************************************************************************"
              + "\nwaiting for btc json data"
              + "\nmsg.to: " + msg.to
              + "\nbtcWalletQry: " + btcWalletQry
              + "\n*********************************************************************************"
            )
          )          
        }
        case PostedExpr( (PostedExpr( btcWalletAddress : String ), _, _, _ ) ) => {
          println(
            (
              "*********************************************************************************"
              + "\nreceived btc json data"
              + "\nmsg.to: " + msg.to
              + "\nbtcWalletQry: " + btcWalletQry
              + "\nbtcWalletAddress: " + btcWalletAddress
              + "\n*********************************************************************************"
            )
          )
          BasicLogService.tweet(
            (
              "*********************************************************************************"
              + "\nreceived btc json data"
              + "\nmsg.to: " + msg.to
              + "\nbtcWalletQry: " + btcWalletQry
              + "\nbtcWalletAddress: " + btcWalletAddress
              + "\n*********************************************************************************"
            )
          )

          // create a receiving address for the recipient    
          val crad =
            CreateReceivingAddressData(
              btcWalletAddress,
              btcReceivePaymentCallbackURL().toString
            )
          val cra = CreateReceivingAddress( crad )

          val btcReceivingAddressQry =
            fromTermString( s"""btc( receivingAddress( Address ) )""" ).get

          ask(
            msg.to,
            btcReceivingAddressQry,
            cra,
            ( optRsrc : Option[mTT.Resource] ) => println( "blockchain response: " + optRsrc )
          )
    
          // issue payment from the supporter
        }
        case _ => {
          println(
            (
              "*********************************************************************************"
              + "\nunexpected btc json data format" + v
              + "\nmsg.to: " + msg.to
              + "\nbtcWalletQry: " + btcWalletQry
              + "\n*********************************************************************************"
            )
          )
          BasicLogService.tweet(
            (
              "*********************************************************************************"
              + "\nunexpected btc json data format" + v
              + "\nmsg.to: " + msg.to
              + "\nbtcWalletQry: " + btcWalletQry
              + "\n*********************************************************************************"
            )
          )
          throw new Exception( "unexpected btc json data format" + v )
        }
      }
    }
    
    def onRead( optRsrc : Option[mTT.Resource] ) : Unit = {      
      optRsrc match {
        case None => ();
        case Some(mTT.Ground( v )) => {
          handleRsp( v )
        }
        case Some(mTT.RBoundHM(Some(mTT.Ground( v )), _)) => {
          handleRsp( v )
        }
      }
    }    

    read( btcWalletQry, List( msg.to ), onRead )
  }
  
  def handleReceivingAddressResponse(
    msg : receivingAddressResponse
  ) : Unit = {
  }

  def handlePaymentNotification(
    msg : receivingAddressResponse
  ) : Unit = {
  }
}
