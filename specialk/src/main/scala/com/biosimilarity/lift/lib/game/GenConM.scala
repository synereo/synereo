// -*- mode: Scala;-*- 
// Filename:    GenConM.scala 
// Authors:     lgm                                                    
// Creation:    Wed May 11 08:58:04 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.game
import com.biosimilarity.lift.lib.collection.{ DeCantor => Set, _ }
import com.biosimilarity.lift.lib.monad._

trait GenConPreM[A] {
  def gcgBind [S,T] (
    ls : GeneralizedConwayGame[S],
    f : S => GeneralizedConwayGame[T]
  ) : GeneralizedConwayGame[T] = {
    GenConGame(
      ( ( Set.empty[Either[T,GeneralizedConwayGame[T]]] : Set[Either[T,GeneralizedConwayGame[T]]] ) /: ls.left )( 
	{
	  ( acc, e ) => {
	    acc ++ List(
	      Right(
		e match {
		  case Right( g ) => gcgBind( g, f )
		  case Left( s ) => f( s )
		}
	      )
	    )
	  }
	}
      ),
      ( ( Set.empty[Either[T,GeneralizedConwayGame[T]]] : Set[Either[T,GeneralizedConwayGame[T]]] ) /: ls.right )( 
	{
	  ( acc, e ) => {
	    acc ++ List(
	      Right(
		e match {
		  case Right( g ) => gcgBind( g, f )
		  case Left( s ) => f( s )
		}
	      )
	    )
	  }
	}
      )
    )
  }
  def gcgMfilter [S] (
    ls : GeneralizedConwayGame[S],
    pred : S => Boolean
  ) : GeneralizedConwayGame[S] = {
    GenConGame(
      ( ( Set.empty[Either[S,GeneralizedConwayGame[S]]] : Set[Either[S,GeneralizedConwayGame[S]]] ) /: ls.left )( 
	{
	  (
	    acc : Set[Either[S,GeneralizedConwayGame[S]]],
	    e : Either[S,GeneralizedConwayGame[S]]
	  ) => {
	    e match {
	      case Right( g ) => {
		val fg : GeneralizedConwayGame[S] = 
		  gcgMfilter[S]( g, pred )
		val rg : Either[S,GeneralizedConwayGame[S]] =
		  Right[S,GeneralizedConwayGame[S]]( fg )
		acc ++ List( rg )
	      }
	      case Left( s ) => {
		if ( pred( s ) ) {
		  acc ++ List( Left( s ) )
		}
		else {
		  acc
		}
	      }
	    }
	  }
	}
      ),
      ( ( Set.empty[Either[S,GeneralizedConwayGame[S]]] : Set[Either[S,GeneralizedConwayGame[S]]] ) /: ls.right )( 
	{
	  (
	    acc : Set[Either[S,GeneralizedConwayGame[S]]],
	    e : Either[S,GeneralizedConwayGame[S]]
	  ) => {
	    e match {
	      case Right( g ) => {
		val fg : GeneralizedConwayGame[S] = 
		  gcgMfilter[S]( g, pred )
		val rg : Either[S,GeneralizedConwayGame[S]] =
		  Right[S,GeneralizedConwayGame[S]]( fg )
		acc ++ List( rg )
	      }
	      case Left( s ) => {
		if ( pred( s ) ) {
		  acc ++ List( Left( s ) )
		}
		else {
		  acc
		}
	      }
	    }
	  }
	}
      )
    )
  }
}

class GenConLeftM[A]( )
extends ForNotationAdapter[GeneralizedConwayGame,A]
with BMonad[GeneralizedConwayGame]
with MonadFilter[GeneralizedConwayGame]
with GenConPreM[A] {
  override def unit [S] ( s : S ) : GeneralizedConwayGame[S] = 
    GenConGame[S]( Set( Left[S,GeneralizedConwayGame[S]]( s ) ), Set.empty )
  override def bind [S,T] (
    ls : GeneralizedConwayGame[S],
    f : S => GeneralizedConwayGame[T]
  ) : GeneralizedConwayGame[T] = {
    gcgBind( ls, f )
  }
  override def mfilter [S] (
    ls : GeneralizedConwayGame[S],
    pred : S => Boolean
  ) : GeneralizedConwayGame[S] = {
    gcgMfilter( ls, pred )
  }
}

class GenConRightM[A]( )
extends ForNotationAdapter[GeneralizedConwayGame,A]
with BMonad[GeneralizedConwayGame]
with MonadFilter[GeneralizedConwayGame]
with GenConPreM[A] {
  override def unit [S] ( s : S ) : GeneralizedConwayGame[S] = 
    GenConGame[S]( Set.empty[Either[S,GeneralizedConwayGame[S]]], Set[Either[S,GeneralizedConwayGame[S]]]( Left[S,GeneralizedConwayGame[S]]( s ) ) )
  override def bind [S,T] (
    ls : GeneralizedConwayGame[S],
    f : S => GeneralizedConwayGame[T]
  ) : GeneralizedConwayGame[T] = {
    gcgBind( ls, f )
  }
  override def mfilter [S] (
    ls : GeneralizedConwayGame[S],
    pred : S => Boolean
  ) : GeneralizedConwayGame[S] = {
    gcgMfilter( ls, pred )
  }
}
