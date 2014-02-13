// -*- mode: Scala;-*- 
// Filename:    Trampoline.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 12 12:12:19 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution.utilities

object DieselValueTrampoline {
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
  import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr  

  implicit def rsrc2V[V](
    optRsrc : Option[mTT.Resource]
  ) : Either[V,Boolean] = {
    optRsrc match {
      case None => {
        Right[V,Boolean]( true )
      }
      case Some( mTT.Ground( PostedExpr( v : V ) ) ) => {
        Left[V,Boolean]( v )
      }
      case Some( mTT.RBoundHM( Some( mTT.Ground( PostedExpr( v : V ) ) ), _ ) ) => {
        Left[V,Boolean]( v )
      }
      case Some( mTT.RBoundAList( Some( mTT.Ground( PostedExpr( v : V ) ) ), _ ) ) => {
        Left[V,Boolean]( v )
      }      
      case _ => {
        Right[V,Boolean]( false )
      }
    }
  }
  implicit def v2Rsrc[V]( v : V ) : mTT.Resource = {
    mTT.Ground( PostedExpr( v ) )
  }
}
