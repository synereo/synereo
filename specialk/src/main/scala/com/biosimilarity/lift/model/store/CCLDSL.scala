// -*- mode: Scala;-*- 
// Filename:    CCLDSL.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr  9 22:19:49 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.zipper._
import com.biosimilarity.lift.lib.moniker._
import scala.collection.SeqProxy
import java.net.URI

object CCLDSL {    
  trait StdCnxnCtxtLabel {
    self : CnxnCtxtLabel[Symbol,Symbol,Any] =>
  }

  class StdCnxnCtxtLiteral[L]( val lit : L )
  extends CnxnCtxtLeaf[Symbol,Symbol,Any]( Left( lit ) )
  with StdCnxnCtxtLabel

  class StdCnxnCtxtVariable( val v : Symbol )
  extends CnxnCtxtLeaf[Symbol,Symbol,Any]( Right( v ) )
  with StdCnxnCtxtLabel

  class StdCnxnCtxtBranch(
    val hd : Symbol,
    val body : List[StdCnxnCtxtLabel with Factual]
  ) extends CnxnCtxtBranch[Symbol,Symbol,Any](
    hd,
    body.asInstanceOf[List[CnxnCtxtLabel[Symbol,Symbol,Any] with Factual]]
  ) with StdCnxnCtxtLabel {
    def apply( body : Any* ) : StdCnxnCtxtBranch = {
      new StdCnxnCtxtBranch(
	hd,
	body.toList.map(
	  ( x ) => {
	    x match {
	      case sym : Symbol =>
		new StdCnxnCtxtVariable( sym )
	      case sccl : StdCnxnCtxtLabel with Factual =>
		sccl
	      case l@_ =>
		new StdCnxnCtxtLiteral( l )
	    }
	  }
	)
      )
    }
  }

  def $( s : Symbol ) : StdCnxnCtxtBranch =
    new StdCnxnCtxtBranch( s, Nil )
  def ?( s : Symbol ) : StdCnxnCtxtBranch =
    new StdCnxnCtxtBranch( s, Nil )
  def ?( s : String ) : StdCnxnCtxtBranch =
    new StdCnxnCtxtBranch( Symbol( s ), Nil )
}

object CCLDSLCXQ extends CnxnXQuery[Symbol,Symbol,Any]
 with CnxnCtxtInjector[Symbol,Symbol,Any]
 with UUIDOps
 with Blobify
 with CnxnXML[Symbol,Symbol,Any] {
 }
