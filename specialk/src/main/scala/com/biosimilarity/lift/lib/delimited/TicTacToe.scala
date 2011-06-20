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

abstract class TicTacToe[M1[_]](
  val n : Int,
  val m : Int
) extends SFKTScope[M1] {
  trait Mark
  case object X extends Mark
  case object O extends Mark

  trait Projections[A,B] {
    def _1 : A
    def _2 : B
  }
  case class BoardLocation(
    val x : Int, val y : Int
  ) extends Projections[Int,Int] {
    override def _1 : Int = x
    override def _2 : Int = y
  }
  
  //type Loc = ( Int, Int );
  type Loc = BoardLocation
  type Board = Map[Loc,Mark]

  type MoveFn = Loc => Loc

  case class Cluster( n : Int, loc : Loc ) extends Projections[Int,Loc] {
    override def _1 : Int = n
    override def _2 : Loc = loc
  }

  type Projectable[A,B] = { def _1 : A; def _2 : B }

  val moveLocFn : List[( MoveFn, MoveFn )] =
    List(
      (
	{ ( xy ) => BoardLocation( xy._1 - 1, xy._2 ) },
	{ ( xy ) => BoardLocation( xy._1 + 1, xy._2 ) }
      ),
      (
	{ ( xy ) => BoardLocation( xy._1, xy._2 - 1 ) },
	{ ( xy ) => BoardLocation( xy._1, xy._2 + 1 ) }
      ),
      (
	{ ( xy ) => BoardLocation( xy._1 - 1, xy._2 - 1 ) },
	{ ( xy ) => BoardLocation( xy._1 + 1, xy._2 + 1 ) }
      ),
      (
	{ ( xy ) => BoardLocation( xy._1 - 1, xy._2 + 1 ) },
	{ ( xy ) => BoardLocation( xy._1 + 1, xy._2 - 1 ) }
      )
    )

  def goodLoc( x : Int, y : Int ) : Boolean = {
    (( x >= 0 ) && ( y >= 0 ) && ( x < n ) && ( y < n ))
  }
   
  def extendLoc(
    board : Board, mfn : MoveFn, m : Mark, loc : Loc
  ) : Cluster = {
    def loop(
      n : Int, lloc : Loc, llocp : Loc
    ) : Cluster = {
      if ( goodLoc( llocp._1, llocp._2 ) ) {
	board.get( llocp ) match {
	  case Some( mp ) => {
	    if ( mp == m ) {
	      loop( n + 1, llocp, mfn( llocp ) )
	    }
	    else {
	      Cluster( n, lloc )
	    }
	  }
	  case None => {
	    Cluster( n, lloc )
	  }
	}
      }
      else {
	Cluster( n, lloc )
      }
    }
    loop( 0, loc, mfn( loc ) )
  }
  
  def maxBy [A] (
    lAs : List[A], pred : ( A, A ) => Boolean, ans : A
  ) : A = {    
    ( ans /: lAs )( 
      {
	( acc, e ) => {
	  if ( pred( e, acc ) ) {
	    e
	  }
	  else {
	    acc
	  }
	}
      }
    )
  }

  def maxByFstProj [A,B,T <: Projectable[A,B]] (
    clstrs : List[T], ans : T
  )( implicit ordA : A => Ordering[A] ) : T = {
    maxBy(
      clstrs, 
      {
	( tpl1 : T, tpl2 : T ) => {
	  ordA( tpl1._1 ).gt( tpl1._1, tpl2._1 )
	}
      },
      ans
    )
  }

  implicit def orderMyInts( n : Int ) : Ordering[Int] =
    scala.math.Ordering.Int
  
  def maxCluster(
    board : Board, m : Mark, loc : Loc
  ) : Cluster = {
    def clusterDir( mfn1 : MoveFn, mfn2 : MoveFn ) = {
      val Cluster( n1, end1 ) = extendLoc( board, mfn1, m, loc )
      val Cluster( n2, end2 ) = extendLoc( board, mfn2, m, loc )
      Cluster( n1 + n2 + 1, end1 )
    }
    val cdl = moveLocFn.map( 
      { ( mfnpr ) => clusterDir( mfnpr._1, mfnpr._2 ) }
    )    
    maxByFstProj[Int,Loc,Cluster]( cdl.drop( 1 ), cdl( 0 ) )
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
	yield{ BoardLocation( x, y ) }).toList,
      new HashMap[Loc,Mark]( )
    )
  }
  
  def showBoard(
    board : Board
  ) : String = {
    //println( "showing board: " + board )
    def showRow( i : Int ) : String = {
      ( "" /: (0 to n - 1) )(
	{
	  ( acc, idx ) => {
	    acc + (board.get( BoardLocation( i, idx ) ) match {
	      case Some( X ) => "X"
	      case Some( O ) => "O"
	      case _ => "."
	    })
	  }
	}
      )
    }
    ( "" /: (0 to n - 1) )(
      {
	( acc, idx ) => {
	  acc + "\n" + showRow( idx )
	}
      }
    )
  }
  
  def takeMove(
    p : Mark, loc : Loc, g : Game
  ) : Game = {
    val boardp = g.board + (( loc, p ))
    val Cluster( n, l ) = maxCluster( boardp, p, loc )
    Game(
      (if ( n >= m) { Some( ( l , p ) ) } else { None }),
      g.moves.diff( List( loc ) ),
      boardp
    )
  }

  case class Outcome( w : Int, g : Game )
       extends Projections[Int,Game] {
	 override def _1 : Int = w
	 override def _2 : Game = g
       }

  trait PlayerProc[T[M[_],_],M[_]]
       extends LogicT[T,M,Outcome]
  with LogicTRunner[T,M,Outcome]
  //with SMonadT[T,M,Outcome]
  with FNMonadT[T,M,Outcome]
  {
    //override type TM[A] = T[M,A]
    def p : Mark
    def proc( g : Game ) : TM[Outcome]        
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
	val playerProcTMSMA = playerProc.tmsma
 	import playerProcTMSMA._
	val iga = playerProc.onceC( playerProc.proc( g ) )	  	
	playerProcTMSMA.bind(
	  iga, 
	  {
	    ( oc : Outcome ) => {
	      val Outcome( _, ga ) = oc
	      println( showBoard( ga.board ) )
	      // BUGBUG -- lgm: This is safe, but ugly!!!
	      game(
		otherPlayer,
		player,
		ga
	      ).asInstanceOf[playerProc.TM[Unit]]
	    }
	  }
	)	
      }
    }
  }

  // This really illustrates the difference between Scala and pure
  // functional languages. In Haskell, for example, this is a
  // function. In Scala there is no place for a named function to
  // reside but on a trait, class or object. All of the typing
  // obligations need to be discharged in the context of such.
  trait AI[T[M[_],_],M[_],RTM[A] <: T[M,A]] extends PlayerProc[T,M] {    
    // Utilities
    override type TM[A] = RTM[A]
    def choose [A] ( la : List[A] ) : TM[A] = {
      monadicTMWitness.msum(
	la.map(
	  monadicTMWitness.unit( _ )
	)
      )
    }

    def otherPlayer( p : Mark ) : Mark = {
      p match {
	case X => O
	case O => X
      }
    }

    def scoreWin = Int.MaxValue
    def scoreLose = Int.MinValue

    def estimateState( p : Mark, g : Game ) : Int = {
      g match {
	case Game( Some( ( _, pp ) ), _, _ ) => {
	  if ( p == pp ) {
	    scoreWin
	  }
	  else {
	    scoreLose
	  }
	}
	case Game( _, Nil, _ ) => 0
	case _ => 10
      }
    }

    def opponent ( p : Mark ) : AI[T,M,RTM]

    override def proc( g : Game ) : TM[Outcome] = {
      g match {
	case Game( Some( _ ), _, _ ) =>
	  monadicTMWitness.unit(
	    Outcome( estimateState( p, g ), g )
	  )
	case Game( _, Nil, _ ) =>
	  monadicTMWitness.unit(
	    Outcome( estimateState( p, g ), g )
	  )
	case _ => {
	  val thisTMSMA = tmsma
	  import thisTMSMA._
	  val tma : TM[Outcome] =	    
	    monadicTMWitness.bind(
	      choose( g.moves ),
	      ( m : Loc ) => {
		val gp = takeMove( p, m, g )

		val oc =
		  opponent/*[T,M,RTM]*/(
		    otherPlayer( p )
		  ).proc( gp )

		monadicTMWitness.bind(
		  oc,
		  ( oc : Outcome ) => {
		    monadicTMWitness.unit(
		      Outcome( -( oc.w ), gp )
		    )
		  }
		)
	      }
	    )

	  val options : TM[List[Outcome]] =
	    bagOfNC( Some( 5 ), tma )
	  for(
	    //wbs <- tmsma.toETramp( options )
	    //wbs <- tmsma.toMembrane( options )
	    wbs <- options
	  )
	  yield {
	    maxByFstProj[Int,Game,Outcome]( wbs.drop( 1 ), wbs( 0 ) )	    
	  }
	}
      }
    }    
  }

  trait BetterAI[T[M[_],_],M[_],RTM[A] <: T[M,A]] extends AI[T,M,RTM]
  {
    case class MoveNOutcome( m : Loc, oc : Outcome )
    extends Projections[Loc,Outcome] {
      override def _1 : Loc = m
      override def _2 : Outcome = oc
    }

    def aiLim( dlim : Int )( blim : Int ) : AI[T,M,RTM]
    def dlim : Int
    def blim : Int

    def firstMoveWins( p : Mark, g : Game ) : TM[Option[( Loc, Outcome )]] = {
      monadicTMWitness.bind(
	choose( g.moves ),
	( m : Loc ) => {
	  val gp = takeMove( p, m, g )	  
	  monadicTMWitness.unit(
	    for( ( _, pp ) <- gp.winner; if ( pp == p ) ) yield {
	      ( m, Outcome(scoreWin,gp) )
	    }
	  )
	}	
      )	
    }
    def minmax(
      dlim : Int,
      blim : Int,
      ppSelf : ( Int => Int => AI[T,M,RTM] ),
      p : Mark,
      g : Game
    ) : TM[Outcome] = {      	  
      val tma : TM[Outcome] =	    
	monadicTMWitness.bind(
	  choose( g.moves ),
	  ( m : Loc ) => {
	    val gp = takeMove( p, m, g )
	    if ( dlim <= 0 ) {
	      monadicTMWitness.unit(
		Outcome( estimateState( p, gp ), gp )
	      )
	    }
	    else {
	      val oc =
		ppSelf( (dlim - 1) )( blim ).opponent(
		  otherPlayer( p )
		).proc( gp )
	    
	      monadicTMWitness.bind(
		oc,
		( oc : Outcome ) => {
		  monadicTMWitness.unit(
		    Outcome( -( oc.w ), gp )
		  )
		}
	      )
	    }	    
	  }
	)

      monadicTMWitness.bind(
	bagOfNC( Some( blim ), tma ),
	( wbs : List[Outcome] ) => {
	  monadicTMWitness.unit(
	    maxByFstProj[Int,Game,Outcome]( wbs.drop( 1 ), wbs( 0 ) )
	  )
	}
      )
    }

    override def proc( g : Game ) : TM[Outcome] = {
      g match {
	case Game( Some( _ ), _, _ ) =>
	  monadicTMWitness.unit(
	    Outcome( estimateState( p, g ), g )
	  )
	case Game( _, Nil, _ ) =>
	  monadicTMWitness.unit(
	    Outcome( estimateState( p, g ), g )
	  )
	case _ => {	  	  
	  ifteC(
	    onceC( firstMoveWins( p, g ) ),
	    ( mX : Option[( Loc, Outcome )] ) => {
	      mX match {
		case Some( mO ) => {
		  monadicTMWitness.unit( mO._2 )
		}
		case _ =>
		  throw new Exception( "why are we here?" )
	      }
	    },
	    ifteC(
	      onceC( firstMoveWins( otherPlayer( p ), g ) ),
	      ( mX : Option[( Loc, Outcome )] ) => {
		mX match {
		  case Some( mO ) => {
		    val gp = takeMove( p, mO._1, g )
		
		    val oc =
		      aiLim( (dlim - 1) )( blim ).opponent(
			otherPlayer( p )
		      ).proc( gp )
		    
		    monadicTMWitness.bind(
		      oc,
		      ( oc : Outcome ) => {
			monadicTMWitness.unit(
			  Outcome( -( oc.w ), gp )
			)
		      }
		    )
		  }
		  case _ =>
		    throw new Exception( "why are we here?" )
		}				
	      },
	      minmax( dlim, blim, aiLim, p, g )
	    )
	  )	    	  
	}
      }
    }
  }

  abstract class SFKTAI
  extends LogicTSFKTC[Outcome]
  with AI[SFKT,M1,SFKTC] {    
  }

  // The class below represents the most specific abstract interface
  // to the Min-Max AI algorithm for playing TicTacToe over an
  // arbitrary monad, M. It is as far as we can go until we know
  // more about M. More specifically,
  abstract class RSFKTAI( override val p : Mark )
  extends SFKTAI {
    // this method can't be defined because RSFKTAI is abstract
    // and RSFKTAI is abstract because...
    override def opponent( op : Mark ) : AI[SFKT,M1,SFKTC] /*= {
      new RSFKTAI( op )
    }*/

    object aTMSMA
      extends TMSMA[Outcome] 
      with MonadicSFKTC

    override def tmsma = aTMSMA
    override def monadicTMWitness = tmsma

    // ...this can't be defined until we know more about M.
    override def monadicMWitness : MonadM  
  }
}

class TicTacToeL( override val n : Int, override val m : Int )
	 extends TicTacToe[List]( n, m )
{
  override type MonadM = ListM[Outcome]
  override def monadicMWitness : MonadM = {
    new ListM[Outcome]()
  }

  class RSFKTAI( override val p : Mark )
  extends SFKTAI {
    override type MonadM = ListM[Outcome]
    override def opponent( op : Mark ) : AI[SFKT,List,SFKTC] = {
      new RSFKTAI( op )
    }

    object aTMSMA
      extends TMSMA[Outcome] 
      with MonadicSFKTC

    override def tmsma = aTMSMA
    override def monadicTMWitness = tmsma

    override def monadicMWitness : MonadM = {
      new ListM[Outcome]()
    }
  }
}





