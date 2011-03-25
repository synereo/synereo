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
  type MonadCC <: MonadicCC
  def monadicCCWitness : MonadCC

  def pushPrompt [W] (
    prompt : Prompt[P,M,W], body : CC[P,M,W]
  ) : CC[P,M,W] = {
    def check( ccv : CCV[P,M,W] ) : M[CCV[P,M,W]] = {
      ccv match {
	case e : Iru[P,M,W] => {
	  monadicMWitness.unit( e )
	}
	case Deru( ctx, nbody ) => {
	  (prompt._2)( nbody ) match {
	    case Some( b ) => {
	      b( ctx ).unCC
	    }
	    case _ => {
	      val nctx = {
		( x : CC[P,M,W] ) => {
		  pushPrompt(
		    prompt,
		    ctx( x.asInstanceOf[CC[P,M,Any]] ) 
		  )
		}
	      }
	      monadicMWitness.unit(
		Deru( nctx.asInstanceOf[SubCont[P,M,Any,W]], nbody )
	      )
	    }
	  }
	}
      }
    }
    
    CCC[W](
      monadicMWitness.bind(
	body.unCC,
	check
      )
    )
    
  }
  def takeSubCont [X,W] (
    prompt : Prompt[P,M,W], body : CCT[P,M,X,W]
  ) : CC[P,M,X] = {
    val nctx = ( x : CC[P,M,X] ) => x
    CCC[X](
      monadicMWitness.unit(      
	Deru(
	  nctx.asInstanceOf[SubCont[P,M,Any,X]],
	  (prompt._1)( body.asInstanceOf[CCT[P,M,Any,W]] )
	)
      )
    )
  }
  def pushSubCont [A,B] (
    sk : SubCont[P,M,A,B],
    e : CC[P,M,A]
  ) : CC[P,M,B] = {
    sk( e )
  }
  def runCC [A] ( ccpma : CC[P,M,A] ) : M[A] = {
    def check( ccv : CCV[P,M,A] ) : M[A] = {
      ccv match {
	case Iru( a ) => {
	  monadicMWitness.unit( a )
	}
	case _ => {
	  throw new Exception(
	    "Escaping bubble: you have forgotten pushPrompt"
	  )
	}
      }
    }
    monadicMWitness.bind(
      ccpma.unCC,
      check
    )
  }

  def abortP [W] (
    prompt : Prompt[P,M,W], body : CC[P,M,W]
  ) : CC[P,M,Any] = {
    val cct = { ( _ : SubCont[P,M,W,Any] ) => body }
    takeSubCont[Any,W](
      prompt,
      cct.asInstanceOf[CCT[P,M,Any,W]]
    )
  }

  def shiftP [W,A] (
    prompt : Prompt[P,M,W],
    f : (( A => CC[P,M,W] ) => CC[P,M,W] )
  ) : CC[P,M,A] = {
    val cct =
      {		  
	( sk : SubCont[P,M,A,W] ) => {
	  val arg = 
	    {
	      ( c : A ) => {
		pushPrompt(
		  prompt,
		  pushSubCont(
		    sk,
		    monadicCCWitness.unit( c )
		  )
		)
	      }
	    }
	  pushPrompt(
	    prompt,
	    f( arg )
	  )
	}
      }
    takeSubCont( prompt, cct.asInstanceOf[CCT[P,M,A,W]] )
  }
}

trait PromptFlavors {
  trait SingleAnswerScope[W] {
    class SingleAnswerType[M[_],X](
      val cct : CCT[SingleAnswerType,M,Any,W]
    ) extends Prompt[SingleAnswerType,M,W] {
      override def _1 = {
	( cct : CCT[SingleAnswerType,M,Any,W] ) => SingleAnswerType( cct )
      }
      override def _2 = {
	( p : SingleAnswerType[M,Any] ) => Some( cct )
      }
    }
    object SingleAnswerType {
      def apply [M[_],X] (
	cct : CCT[SingleAnswerType,M,Any,W]
      ) : SingleAnswerType[M,X] = {
	new SingleAnswerType[M,X]( cct )
      }
      def unapply [M[_],X] (
	sat : SingleAnswerType[M,X]
      ) : Option[
        (
	  CCT[SingleAnswerType,M,Any,W] => SingleAnswerType[M,Any],
	  SingleAnswerType[M,Any] => Option[CCT[SingleAnswerType,M,Any,W]]
	)
      ] =
	{ 
	  Some( ( sat._1, sat._2 ) )
	}
    }
  }
  
  trait TwoAnswerScope[W1,W2,A] {
    abstract class TwoAnswerTypes[M[_],X](
      val choice : Either[
	CCT[TwoAnswerTypes,M,X,W1],
	CCT[TwoAnswerTypes,M,X,W2]
      ]
    )

    class TwoAnswerTypesL[M[_],X](
      override val choice : Either[
	CCT[TwoAnswerTypes,M,X,W1],
	CCT[TwoAnswerTypes,M,X,W2]
      ]
    ) extends TwoAnswerTypes[M,X]( choice )
	     with Prompt[TwoAnswerTypes,M,W1]
    {
      override def _1 :
      CCT[TwoAnswerTypes,M,Any,W1] => TwoAnswerTypes[M,Any]
      = {
	( cct : CCT[TwoAnswerTypes,M,Any,W1] ) =>
	  TwoAnswerTypesL[M,Any](
	    Left[
	      CCT[TwoAnswerTypes,M,Any,W1],
	      CCT[TwoAnswerTypes,M,Any,W2]
	    ]( cct )
	  )
      }
      override def _2 :
      TwoAnswerTypes[M,Any] => Option[CCT[TwoAnswerTypes,M,Any,W1]]
      = {
	( p : TwoAnswerTypes[M,Any] ) => {
	  p match {
	    case tatl : TwoAnswerTypesL[M,Any] => {
	      tatl.choice match {
		case Left( cct ) => Some( cct )
		case _ => None
	      }
	    }
	    case _ => None
	  }
	}
      }
    }

    object TwoAnswerTypesL {
      def apply [M[_],X] (
	cct : Either[
	  CCT[TwoAnswerTypes,M,X,W1],
	  CCT[TwoAnswerTypes,M,X,W2]
	]
      ) : TwoAnswerTypesL[M,X] = {
	new TwoAnswerTypesL[M,X]( cct )
      }
      def unapply [M[_],X] (
	sat : TwoAnswerTypesL[M,X]
      ) : Option[
        (
	  CCT[TwoAnswerTypes,M,Any,W1] => TwoAnswerTypes[M,Any],
	  TwoAnswerTypes[M,Any] => Option[CCT[TwoAnswerTypes,M,Any,W1]]
	)
      ] =
	{ 
	  Some( ( sat._1, sat._2 ) )
	}
    }

    class TwoAnswerTypesR[M[_],X](
      override val choice : Either[
	CCT[TwoAnswerTypes,M,X,W1],
	CCT[TwoAnswerTypes,M,X,W2]
      ]
    ) extends TwoAnswerTypes[M,X]( choice )
	     with Prompt[TwoAnswerTypes,M,W2] {
      override def _1 :
      CCT[TwoAnswerTypes,M,Any,W2] => TwoAnswerTypes[M,Any]
      = {
	( cct : CCT[TwoAnswerTypes,M,Any,W2] ) =>
	  TwoAnswerTypesR[M,Any](
	    Right[
	      CCT[TwoAnswerTypes,M,Any,W1],
	      CCT[TwoAnswerTypes,M,Any,W2]
	    ]( cct )
	  )
      }
      override def _2 :
      TwoAnswerTypes[M,Any] => Option[CCT[TwoAnswerTypes,M,Any,W2]]
      = {
	( p : TwoAnswerTypes[M,Any] ) => {
	  p match {
	    case tatr : TwoAnswerTypesR[M,Any] => {
	      tatr.choice match {
		case Right( cct ) => Some( cct )
		case _ => None
	      }
	    }
	    case _ => None
	  }
	}
      }
    }
    
    object TwoAnswerTypesR {
      def apply [M[_],X] (
	cct : Either[
	  CCT[TwoAnswerTypes,M,X,W1],
	  CCT[TwoAnswerTypes,M,X,W2]
	]
      ) : TwoAnswerTypesR[M,X] = {
	new TwoAnswerTypesR[M,X]( cct )
      }
      def unapply [M[_],X] (
	sat : TwoAnswerTypesR[M,X]
      ) : Option[
        (
	  CCT[TwoAnswerTypes,M,Any,W2] => TwoAnswerTypes[M,Any],
	  TwoAnswerTypes[M,Any] => Option[CCT[TwoAnswerTypes,M,Any,W2]]
	)
      ] =
	{ 
	  Some( ( sat._1, sat._2 ) )
	}
    }
  }
}
