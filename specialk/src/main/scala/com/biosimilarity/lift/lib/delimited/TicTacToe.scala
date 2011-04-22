// -*- mode: Scala;-*- 
// Filename:    TicTacToe.scala 
// Authors:     lgm                                                    
// Creation:    Fri Apr 22 03:58:53 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.delimited

import com.biosimilarity.lift.lib.monad._

import scala.collection.immutable.Map
import scala.collection.immutable.HashMap

class TicTacToe( n : Int, m : Int ) {
  trait Mark
  case object X extends Mark
  case object O extends Mark
  
  type Loc = ( Int, Int );
  type Board = Map[Loc,Mark]

  type MoveFn = Loc => Loc

  val moveLocFn : List[( MoveFn, MoveFn )] =
    List(
      ( { ( xy ) => ( xy._1 - 1, xy._2 ) }, { ( xy ) => ( xy._1 + 1, xy._2 ) } ),
      ( { ( xy ) => ( xy._1, xy._2 - 1 ) }, { ( xy ) => ( xy._1, xy._2 + 1 ) } ),
      ( { ( xy ) => ( xy._1 - 1, xy._2 - 1 ) }, { ( xy ) => ( xy._1 + 1, xy._2 + 1 ) } ),
      ( { ( xy ) => ( xy._1 - 1, xy._2 + 1 ) }, { ( xy ) => ( xy._1 + 1, xy._2 - 1 ) } )
    )

  def goodLoc( x : Int, y : Int ) : Boolean = {
    (( x >= 0 ) && ( y >= 0 ) && ( x < n ) && ( y < n ))
  }
   
  def extendLoc(
    board : Board, mfn : MoveFn, m : Mark, loc : Loc
  ) : ( Int, Loc ) = {
    def loop(
      n : Int, lloc : Loc, llocp : Loc
    ) : ( Int, Loc ) = {
      if ( goodLoc( llocp._1, llocp._2 ) ) {
	board.get( llocp ) match {
	  case Some( mp ) => {
	    if ( mp == m ) {
	      loop( n + 1, llocp, mfn( llocp ) )
	    }
	    else {
	      ( n, lloc )
	    }
	  }
	  case None => {
	    ( n, lloc )
	  }
	}
      }
      else {
	( n, lloc )
      }
    }
    loop( 0, loc, mfn( loc ) )
  }
  
  def maxCluster(
    board : Board, m : Mark, loc : Loc
  ) : ( Int, Loc ) = {
    def clusterDir( mfn1 : MoveFn, mfn2 : MoveFn ) = {
      val ( n1, end1 ) = extendLoc( board, mfn1, m, loc )
      val ( n2, end2 ) = extendLoc( board, mfn2, m, loc )
      ( n1 + n2 + 1, end1 )
    }
    val cdl = moveLocFn.map( 
      { ( mfnpr ) => clusterDir( mfnpr._1, mfnpr._2 ) }
    )
    def maximumBy(
      clstrs : List[( Int, Loc )], ans : ( Int, Loc )
    ) : ( Int, Loc ) = {
      ( clstrs, ans ) match {
	case ( ( n, loc ) :: nclstrs, ( an, aloc ) ) => {
	  if ( n > an ) {
	    maximumBy( nclstrs, ( n, loc ) )
	  }
	  else {
	    maximumBy( nclstrs, ( an, aloc ) )
	  }
	}
	case ( Nil, _ ) => ans
      }
    }
    maximumBy( cdl.drop( 1 ), cdl( 0 ) )
  }

  case class Game(
    winner : Option[( Loc, Mark )],
    moves : List[Loc],
    board : Board
  )

  def newGame : Game = {
    Game(
      None,
      (for( x <- 0 to n - 1; y <- 0 to n - 1 ) 
	yield{ ( x, y ) }).toList,
      new HashMap[Loc,Mark]( )
    )
  }
  
  def showBoard(
    board : Board
  ) : String = {
    def showRow( i : Int ) : Unit = {
      ( "" /: (0 to n - 1) )(
	{
	  ( acc, idx ) => {
	    acc + (board.get( ( i, idx ) ) match {
	      case Some( X ) => "X"
	      case Some( O ) => "O"
	      case _ => " ."
	    })
	  }
	}
      )
    }
    ( "" /: (0 to n - 1) )(
      {
	( acc, idx ) => {
	  acc + showRow( idx )
	}
      }
    )
  }
  
  def takeMove(
    p : Mark, loc : Loc, g : Game
  ) : Game = {
    val boardp = g.board + (( loc, p ))
    val ( n, l ) = maxCluster( boardp, p, loc )
    Game(
      (if ( n >= m) { Some( ( l , p ) ) } else { None }),
      g.moves.diff( List( loc ) ),
      boardp
    )
  }

  trait PlayerProc[T[M[_],_],M[_]]
       extends LogicT[T,M,( Int, Game )]
  with SMonadT[T,M,( Int, Game )] {
    def p : Mark
    def proc : Game => TM[( Int, Game )]
  }

  def game [T[M[_],_],M[_]] (
    player1 : ( Mark, PlayerProc[T,M] ),
    player2 : ( Mark, PlayerProc[T,M] )
  ) : T[M,Unit] = {
    game( player1, player2, newGame )
  }
  
  def game [T[M[_],_],M[_]] (
    player : ( Mark, PlayerProc[T,M] ),
    otherPlayer : ( Mark, PlayerProc[T,M] ),
    g : Game
  ) : T[M,Unit] = {
    val playerProc = player._2
    g match {
      case Game( Some( k ), _, _ ) => {
	playerProc.monadicTMWitness.unit(
	  println( k + " wins!" )
	)
      }
      case Game( _, Nil, _ ) => {
	playerProc.monadicTMWitness.unit(
	  println( "Draw!" )
	)
      }
      case _ => {	
        val ( _, ga ) = playerProc.onceC( playerProc.proc( g ) )
	val gp = ga.asInstanceOf[Game]
	println( showBoard( gp.board ) )
	game( otherPlayer, player, gp )
      }
    }
  }
}





