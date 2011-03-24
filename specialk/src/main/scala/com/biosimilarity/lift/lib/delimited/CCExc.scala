// -*- mode: Scala;-*- 
// Filename:    CCExc.scala 
// Authors:     lgm                                                    
// Creation:    Thu Mar 17 14:47:04 2011 
// Copyright:   Not supplied 
// Description: 
// This is a transliteration of Oleg's implementation of the monadic
// presentation of delimited control
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.delimited

import com.biosimilarity.lift.lib.monad._

trait CCV[P[M[_],A],M[_],A]
case class Iru[P[M[_],A],M[_],A]( a : A ) extends CCV[P,M,A]
case class Deru[P[M[_],A],M[_],A](
  ctx : SubCont[P,M,Any,A],
  body : P[M,Any]
) extends CCV[P,M,A]

trait CC[P[M[_],_],M[_],A]{
  def unCC : M[CCV[P,M,A]]
}

trait SubCont[P[M[_],_],M[_],A,B]
     extends Function1[CC[P,M,A],CC[P,M,B]]

trait CCT[P[M[_],_],M[_],A,W]
     extends Function1[SubCont[P,M,A,W],CC[P,M,W]]

trait Prompt[P[M[_],_],M[_],W] {
  def _1 : CCT[P,M,Any,W] => P[M,Any]
  def _2 : P[M,Any] => Option[CCT[P,M,Any,W]]
}

trait MonadicCCScope[P[M[_],_],M[_]] {
  type MonadM <: BMonad[M]
  def monadicMWitness : MonadM
  
  case class CCC[A]( 
    override val unCC : M[CCV[P,M,A]]
  ) extends CC[P,M,A]  

  abstract class MonadicCC
	   extends BMonad[CCC]
  {
    override def unit [A] ( a : A ) : CCC[A] = {
      CCC( monadicMWitness.unit( Iru( a ) ) )
    }
    override def bind [A,B] (
      ma : CCC[A], f : A => CCC[B]
    ) : CCC[B] = {
      def check ( ccv : CCV[P,M,A] ) : M[CCV[P,M,B]] = {
	ccv match {
	  case Iru( a ) => {
	    f( a ).unCC
	  }
	  case Deru( ctx, body ) => {
	    val nctx =
	      ( x : CC[P,M,Any] ) => {
		bind [A,B](
		  ctx( x ).asInstanceOf[CCC[A]],
		  f
		)
	      }

	    monadicMWitness.unit(
	      Deru(
		nctx.asInstanceOf[SubCont[P,M,Any,B]],
		body
	      )
	    )
	  }
	}
      }
      
      CCC(
	monadicMWitness.bind(
	  ma.unCC,
	  check
	)
      )
    }    
  }
}

trait MonadTransformerCCScope[P[M[_],_],M[_],A]
extends MonadicCCScope[P,M] {
  trait CCMT[M[_],A] extends CC[P,M,A]
  
  case class CCCMT[M[_],A]( 
    override val unCC : M[CCV[P,M,A]]
  ) extends CCMT[M,A]

  abstract class MonadTransformerCC
  extends MonadT[CCMT,M] {
    override def lift [A] ( ma : M[A] ) : CCMT[M,A] = {
      CCCMT(
	monadicMWitness.bind(
	  ma,
	  ( a : A ) => monadicMWitness.unit( Iru( a ) )
	)
      )
    }
  }
}

trait DelimitedControl[P[M[_],_],M[_],A] 
	   extends MonadicCCScope[P,M]
{
  def pushPrompt [W] (
    prompt : Prompt[P,M,W], body : CC[P,M,W]
  ) : CC[P,M,W]
  def takeSubCont [X,W] (
    prompt : Prompt[P,M,W], body : CCT[P,M,X,W]
  )
  def pushSubCont [A,B] (
    sk : SubCont[P,M,A,B],
    e : CC[P,M,A]
  ) : CC[P,M,B]
  def runCC [A] ( ccpma : CC[P,M,A] ) : M[A]
}

trait PromptFlavors {
  trait SingleAnswerScope[W] {
    case class SingleAnswerType[M[_],X](
      cct : CCT[SingleAnswerType,M,Any,W]
    ) extends Prompt[SingleAnswerType,M,W] {
      override def _1 = {
	( cct : CCT[SingleAnswerType,M,Any,W] ) => SingleAnswerType( cct )
      }
      override def _2 = {
	( p : SingleAnswerType[M,Any] ) => Some( cct )
      }
    }
  }
  trait TwoAnswerScope[M[_],X,A] {
    abstract class TwoAnswerTypes[W1[_],W2[_]](
      val choice : Either[
	CCT[TwoAnswerTypes,M,Any,W1[A]],
	CCT[TwoAnswerTypes,M,Any,W2[A]]
      ]
    )
    abstract class TwoAnswerTypesL[W1[_],W2[_]](
      override val choice : Either[
	CCT[TwoAnswerTypes,M,Any,W1[A]],
	CCT[TwoAnswerTypes,M,Any,W2[A]]
      ]
    ) extends TwoAnswerTypes[W1,W2]( choice )
	     with Prompt[TwoAnswerTypes,M,W1[A]]
    {
      override def _1 :
      CCT[TwoAnswerTypes,M,Any,W1[A]] => TwoAnswerTypes[M,Any]
      // = {
// 	( cct : CCT[TwoAnswerTypes,M,Any,W1[A]] ) =>
// 	  TwoAnswerTypesL[M,Any](
// 	    Left[
// 	      CCT[TwoAnswerTypes,M,Any,M[A]],
// 	      CCT[TwoAnswerTypes,M,Any,Any]
// 	    ]( cct )
// 	  )
//       }
      override def _2 :
      TwoAnswerTypes[M,Any] => Option[CCT[TwoAnswerTypes,M,Any,W1[A]]]
      // = {
// 	( p : TwoAnswerTypes[M,Any] ) => {
// 	  p match {
// 	    case TwoAnswerTypesL( Left( cct ) ) => {
// 	      Some( p.cct )
// 	    }
// 	    case _ => None
// 	  }
// 	}
//       }
    }
    abstract class TwoAnswerTypesR[W1[_],W2[_]](
      override val choice : Either[
	CCT[TwoAnswerTypes,M,Any,W1[A]],
	CCT[TwoAnswerTypes,M,Any,W2[A]]
      ]
    ) extends TwoAnswerTypes[W1,W2]( choice )
	     with Prompt[TwoAnswerTypes,M,W2[A]] {
      override def _1 :
      CCT[TwoAnswerTypes,M,Any,W2[A]] => TwoAnswerTypes[M,Any]
      // = {
// 	( cct : CCT[TwoAnswerTypes,M,Any,W2[A]] ) =>
// 	  TwoAnswerTypesR[M,Any](
// 	    Right[
// 	      CCT[TwoAnswerTypes,M,Any,Any],
// 	      CCT[TwoAnswerTypes,M,Any,M[A]]
// 	    ]( cct )
// 	  )
//       }
      override def _2 :
      TwoAnswerTypes[M,Any] => Option[CCT[TwoAnswerTypes,M,Any,W2[A]]]
      // = {
// 	( p : TwoAnswerTypes[M,Any] ) => {
// 	  p match {
// 	    case TwoAnswerTypesR( Right( cct ) ) => {
// 	      Some( p.cct )
// 	    }
// 	    case _ => None
// 	  }
// 	}
//       }
    }
  }
}
