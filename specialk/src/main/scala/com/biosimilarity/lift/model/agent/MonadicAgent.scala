// -*- mode: Scala;-*- 
// Filename:    MonadicAgent.scala 
// Authors:     lgm                                                    
// Creation:    Fri Jan 21 16:26:16 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.agent

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.model.msg._

import scala.util.continuations._ 
import scala.collection.mutable._

import java.net.URI
import java.util.UUID

trait MonadicAgency[TxPort,ReqBody,RspBody]
 extends Socialite[ReqBody,RspBody]
	 with Awareness[ReqBody,RspBody]
	 with Focus[ReqBody,RspBody]
{
  self : MonadicWireToTrgtConversion with MonadicGenerators with WireTap =>
  
  type Wire = TxPort
  type Trgt =
    Either[JustifiedRequest[ReqBody,RspBody],JustifiedResponse[ReqBody,RspBody]]  
    
    override def useBraceNotation : Boolean = false
  //def likes( dsg : URI, acq : Socialite[ReqBody,RspBody] )
  def likes( dsg : Moniker, acq : Socialite[ReqBody,RspBody] )
  : Boolean = true
       
  override def handleRequestPayload ( payload : ReqBody )
  : Boolean = false
  override def handleResponsePayload ( payload : RspBody )
  : Boolean = false  

  def dispatch(
    msgGenerator : Generator[TxPort,Unit,Unit]
  ) = 
    Generator {
      k : ( Trgt => Unit @suspendable ) =>
	shift {
	  outerK : ( Unit => Unit ) =>
	    reset {
	      for( msg <- xformAndDispatch( msgGenerator ) ) {
		msg match {
		  case l@Left(
		    jreq@JustifiedRequest(
		      m, p, d, t,
		      f : ReqBody,
		      c : Option[Response[AbstractJustifiedRequest[ReqBody,RspBody],RspBody]]
		    )
		  ) => {
		    if ( validate( jreq ) ) {
		      BasicLogService.reportage( "calling handler on " + jreq )
		      k( l )
		    }
		  }
		  case r@Right(
		    jrsp@JustifiedResponse(
		      m, p, d, t,
		      f : RspBody,
		      c : Option[Request[AbstractJustifiedResponse[ReqBody,RspBody],ReqBody]]
		    )
		  ) => {
		    if ( validate( jrsp ) ) {
		      BasicLogService.reportage( "calling handler on " + jrsp )
		      k( r )
		    }
		  }
		}

	      }

	      BasicLogService.reportage( "dispatch returning" )
  	      outerK()
	    }
	}
    }
}
