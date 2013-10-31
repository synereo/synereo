// -*- mode: Scala;-*- 
// Filename:    TSpaceDesignPattern.scala 
// Authors:     lgm                                                    
// Creation:    Mon Sep  6 17:57:30 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._

import scala.util.continuations._ 
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer

trait RetentionPolicy extends Serializable
trait RetainInCache extends RetentionPolicy
trait RetainInStore extends RetentionPolicy
trait Subscription extends RetentionPolicy
case object DoNotRetain extends RetentionPolicy 
case object Cache extends RetainInCache 
case object Store extends RetainInStore 
case object CacheAndStore extends RetainInCache with RetainInStore
case object CacheAndStoreSubscription extends RetainInCache
 with RetainInStore with Subscription

class SpaceLock[RK](
  val readingRoom : HashMap[RK,Boolean],
  val writingRoom : Buffer[Int],
  val history : Buffer[RK],
  val maxWROccupancy : Int,
  val maxRROccupancy : Int
) {
  def allowedIn( rk : RK ) : Boolean = {
    BasicLogService.tweet(
      "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
      + "\nin method: SpaceLock.allowedIn(rk)"
      + "\nthis : " + this
      + "\nrk : " + rk
      + "\nwritingRoom : " + writingRoom
      + "\nreadingRoom : " + readingRoom
      + "\n+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
    )
    if ( writingRoom.size < 1 ) {
      readingRoom += ( rk -> true )
      true
    }
    else {
      writingRoom( 0 ) match {
        case 0 => {       
          readingRoom += ( rk -> true )
          true
        }
        case _ => false
      }
    }
  }
  
  def allowedIn() : Boolean = {
    BasicLogService.tweet(
      "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
      + "\nin method: SpaceLock.allowedIn()"
      + "\nthis : " + this
      + "\nwritingRoom : " + writingRoom
      + "\nreadingRoom : " + readingRoom
      + "\nmaxWROccupancy" + maxWROccupancy
      + "\n+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
    )
    readingRoom.keys match {
      case Nil => {
        writingRoom.size match {
          case 0 => {
            writingRoom += 1
          }
          case 1 => {
            writingRoom( 0 ) += 1       
          }
          case _ => {
            throw new Exception( "oddly shaped writing room: " + writingRoom )
          }
        }
        ( writingRoom( 0 ) <= maxWROccupancy )
      }
      case _ => {
        val p =
          ( false /: readingRoom.values )( 
            { ( acc, b ) => { acc || b } }
          )
        if ( !p ) {
          writingRoom.size match {
            case 0 => {
              writingRoom += 1
            }
            case 1 => {
              writingRoom( 0 ) += 1
            }
            case _ => {
              throw new Exception( "oddly shaped writing room: " + writingRoom )
            }
          }
          ( writingRoom( 0 ) <= maxWROccupancy )
        }
        else {
          false
        }
      }
    }
  }
  
  def leave() : Unit = {
    if ( writingRoom.size > 0 ) {
      writingRoom( 0 ) match {
        case 1 => {
          writingRoom.clear
          notify()
        }
        case i : Int => {
          writingRoom( 0 ) -= 1
        }
        case _ => {
          throw new Exception( "leaving writing room without entering" )
        }
      }
    }
    else {
      throw new Exception( "leaving writing room without entering" )
    }
  }
  
  def leave( rk : RK ) : Unit = {
    readingRoom.get( rk ) match {
      case Some( false ) => {
        readingRoom -= rk
        history += rk
      }
      case Some( true ) => {
        readingRoom += ( rk -> false )
      }
      case _ => {
        history.contains( rk ) match {
          case true => {
          }
          case false => {
            //throw new Exception( "leaving reading room without entering: " + rk )
          }
        }       
      }
    }
    readingRoom.keys match {
      case Nil => notify()
      case _ => {
        val p =
          ( false /: readingRoom.values )( 
            { ( acc, b ) => { acc || b } }
          )
        if ( !p ) { notify() }
      }
    }
  }
  
  def occupy( ork : Option[RK] ) = synchronized {
    // We test and set in one operation
    ork match {
      case Some( rk ) => {
        while ( ! allowedIn( rk ) ) wait()
      }
      case None => {
        while ( ! allowedIn() ) wait()
      }
    }      
  }
  
  def depart( ork : Option[RK] ) = synchronized {
    ork match {
      case Some( rk ) => leave( rk )
      case None => leave()
    }
    
  }
}

object SpaceLock {
  def apply [RK] (
    readingRoom : HashMap[RK,Boolean],
    writingRoom : Buffer[Int],
    history : Buffer[RK],
    maxWROccupancy : Int,
    maxRROccupancy : Int 
  ) : SpaceLock[RK] = {
    new SpaceLock[RK](
      readingRoom, writingRoom, history, maxWROccupancy, maxRROccupancy
    )
  }
  def unapply [RK] (
    sl : SpaceLock[RK]
  ) : Option[( HashMap[RK,Boolean], Buffer[Int], Buffer[RK], Int, Int )] = {
    Some(
      (
        sl.readingRoom,
        sl.writingRoom,
        sl.history,
        sl.maxWROccupancy,
        sl.maxRROccupancy
      )
    )
  }
}

trait Mode[RK,Pattern] {
  def ptn : Pattern
  def optRK : Option[RK]
}

case class Station[RK,Pattern](
  @transient override val ptn : Pattern,
  @transient override val optRK : Option[RK]
) extends Mode[RK,Pattern]

case class HigherStation[RK,Pattern](
  @transient override val ptn : Pattern,
  @transient override val optRK : Option[RK],
  polarity : Boolean
) extends Mode[RK,Pattern]

trait ModeSpaceLock[RK,Pattern] {
  type ModeType <: Mode[RK,Pattern]
  def locker : HashMap[ModeSpaceLock[RK,Pattern]#ModeType,Int]
  def maxOccupancy : Int  
  /*
   * Vibratory mode construction
   */
  def makeMode( ptn : Pattern, rk : Option[RK] ) : ModeType
  def makeMode( ptn : Pattern, rk : RK ) : ModeType
  def makeMode( ptn : Pattern ) : ModeType
  
  def allowedIn( s : ModeType ) : Boolean = {
    BasicLogService.tweet(
      "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
      + "\nin method: ModeSpaceLock.allowedIn(s)"
      + "\nthis : " + this
      + "\ns : " + s
      + "\nlocker : " + locker
      + "\n+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
    )
    locker.get( s ) match {
      case Some( i ) => {
        if ( i < maxOccupancy ) {
          locker += ( s -> ( i + 1 ) )
          true
        }
        else {
          false
        }
      }
      case None => {
        locker += ( s -> 1 )
        true
      }
    }
  }
  def leave( s : ModeType ) : Unit = {
    locker.get( s ) match {
      case Some( 1 ) => {
        locker -= s
        notify()
      }
      case Some( i ) => {
        locker += ( s -> ( i - 1 ) )
        notify()
      }
      case None => {
        /* throw new Exception */ //println( "leaving lock without entering" )
      }
    }
  }
  /*
   * Occupation of a vibratory mode
   */
  def occupy( s : ModeType ) : Unit = synchronized {
    while ( ! allowedIn( s ) ) wait()
  }
  def occupy( ptn : Pattern ) : Unit = {
    occupy( makeMode( ptn ) )
  }
  def occupy( ptn : Pattern, rk : RK ) : Unit  = {
    occupy( makeMode( ptn, rk ) )
  }
  def occupy( ptn : Pattern, optRK : Option[RK] ) : Unit  = {
    occupy( makeMode( ptn, optRK ) )
  }
  /*
   * Departure of a vibratory mode
   */
  def depart( s : ModeType ) : Unit = synchronized {
    leave( s )
  }
  def depart( ptn : Pattern ) : Unit = {
    depart( makeMode( ptn ) )
  }
  def depart( ptn : Pattern, rk : RK ) : Unit = {
    depart( makeMode( ptn, rk ) )
  }
  def depart( ptn : Pattern, optRK : Option[RK] ) : Unit = {
    depart( makeMode( ptn, optRK ) )
  }
}

case class KeyRKSpaceLock[RK,Pattern](
  @transient override val locker : HashMap[ModeSpaceLock[RK,Pattern]#ModeType,Int],
  override val maxOccupancy : Int
) extends ModeSpaceLock[RK,Pattern] {  
  override type ModeType = Station[RK,Pattern]
  def makeMode( ptn : Pattern, rk : Option[RK] ) : ModeType = {
    Station[RK,Pattern]( ptn, rk )
  }
  def makeMode( ptn : Pattern, rk : RK ) : ModeType = {
    Station[RK,Pattern]( ptn, Some( rk ) )
  }
  def makeMode( ptn : Pattern ) : ModeType = {
    Station[RK,Pattern]( ptn, None )
  }
}

case class KeyNoKSpaceLock[RK,Pattern](
  @transient override val locker : HashMap[ModeSpaceLock[RK,Pattern]#ModeType,Int],
  override val maxOccupancy : Int
) extends ModeSpaceLock[RK,Pattern] {  
  override type ModeType = Station[RK,Pattern]
  def makeMode( ptn : Pattern, rk : Option[RK] ) : ModeType = {
    Station[RK,Pattern]( ptn, None )
  }
  def makeMode( ptn : Pattern, rk : RK ) : ModeType = {
    Station[RK,Pattern]( ptn, None )
  }
  def makeMode( ptn : Pattern ) : ModeType = {
    Station[RK,Pattern]( ptn, None )
  }
}

trait ExcludedMiddleTypes[Place,Pattern,Resource] {
  type RK = Option[Resource] => Unit @suspendable  

  class BlockableContinuation(
    val rk : RK
  ) extends Function1[Option[Resource],Unit @suspendable] {
    override def apply(
      orsrc : Option[Resource]
    ) : Unit @suspendable = {
      rk( orsrc )
    }
    def unapply(
      bc : BlockableContinuation
    ) : Option[(RK)] = {
      Some( ( bc.rk ) )
    }
    def blockForValue() = {
      synchronized {
        wait()
      }
    }
    def unblockOnValue() = {
      synchronized {
        notifyAll()
      }
    }
  }

  type Substitution <: Function1[Resource,Option[Resource]]

  case class IdentitySubstitution( )
       extends Function1[Resource,Option[Resource]] {
         override def apply( rsrc : Resource ) = Some( rsrc )
       }

  case class PlaceInstance(
    place : Place,
    stuff : Either[Resource,List[RK]],
    subst : Substitution
  )
}

trait ExcludedMiddleScope[Place,Pattern,Resource] {
  type EMTypes <: ExcludedMiddleTypes[Place,Pattern,Resource]
  def protoEMTypes : EMTypes
  val emT : EMTypes = protoEMTypes
}

trait MonadicTupleSpace[Place,Pattern,Resource]
//       extends MapLike[Place,Resource, This]
extends MonadicGenerators
with ThreadPoolRunnersX
//with FJTaskRunnersX
with ExcludedMiddleTypes[Place,Pattern,Resource]
{
  self : WireTap
      with ConfigurationTrampoline =>

  @transient
  //var _spaceLock : Option[SpaceLock[RK]] = None
  var _spaceLock : Option[ModeSpaceLock[RK,Pattern]] = None

  // def spaceLock : SpaceLock[RK] = {
//     _spaceLock match {
//       case Some( sl ) => sl
//       case null => {
//      val sl =
//        new SpaceLock[RK](
//          new HashMap[RK, Boolean](),
//          new ListBuffer[Int](),
//          new ListBuffer[RK],
//          1,
//          1
//        )
//      _spaceLock = Some( sl )
//      sl
//       }
//       case None => {
//      val sl =
//        new SpaceLock[RK](
//          new HashMap[RK, Boolean](),
//          new ListBuffer[Int](),
//          new ListBuffer[RK],
//          1,
//          1
//        )
//      _spaceLock = Some( sl )
//      sl
//       }
//     }
//   }

  def spaceLock : ModeSpaceLock[RK,Pattern] = {
    _spaceLock match {
      case Some( sl ) => sl
      case None | null => {
        val sl =
          KeyNoKSpaceLock[RK,Pattern](
            new HashMap[ModeSpaceLock[RK,Pattern]#ModeType,Int](),
            1
          )
        _spaceLock = Some( sl )
        sl
      }
    }
  }

  def theMeetingPlace : Map[Place,Resource]
  def theChannels : Map[Place,Resource]
  def theWaiters : Map[Place,List[RK]]
  def theSubscriptions : Map[Place,List[RK]]

  // comm( fits : ( Pattern, Pattern ) => Boolean ): 
  // fits( t, s ) => t?( x )P | s!( y ) -> P{ y/x }
  // commK( fits : ( Pattern, Pattern ) => Boolean ):
  // fits( t, s ), unifies( u, v, s ) => 
  // t?( u )P | s!( v ) -> Ps

  def fits( ptn : Pattern, place : Place ) : Boolean
  def fitsK( ptn : Pattern, place : Place ) : Option[Substitution]
  def representative( ptn : Pattern ) : Place  

  // def spawn(ctx: =>(Any @cps[Unit]))(implicit sched: AbstractTaskRunner): Unit = {
//     if ( sched != null ) {
//       sched.submitTask(() => run(ctx))
//     }
//     else {
//       BasicLogService.tweet( "warning: implicit argument, sched, is null" )
//       createDefaultTaskRunner().submitTask(() => run(ctx))
//     }
//   }

  //def self = theMeetingPlace

  override def itergen[T]( coll : Iterable[T] ) = 
    Generator {
      gk : ( T => Unit @suspendable ) =>
        val collItr = coll.iterator

        while( collItr.hasNext ) {
          gk( collItr.next )
        }
    }
  
  def locations(
    map : Either[Map[Place,Resource],Map[Place,List[RK]]],
    ptn : Pattern
  ) : List[PlaceInstance] = {
    def lox[Trgt,ITrgt](
      m : Map[Place,Trgt],
      inj : Trgt => ITrgt
    ) : List[(Place,ITrgt,Substitution)]
    = {
      ( ( Nil : List[(Place,ITrgt,Substitution)] ) /: m )( 
        { 
          ( acc, kv ) => {
            val ( k, v ) = kv
            fitsK( ptn, k ) match {
              case Some( s ) => acc ++ List[(Place,ITrgt,Substitution)]( ( k, inj( v ), s ) )
              case None => acc
            }
          }
        }
      )
    }

    val triples =
      map match {
        case Left( m ) => {
          lox[Resource,Either[Resource,List[RK]]](
            m, ( r ) => Left[Resource,List[RK]]( r )
          )
        }
        case Right( m ) => {
          lox[List[RK],Either[Resource,List[RK]]](
            m, ( r ) => Right[Resource,List[RK]]( r )
          )
        }
      }
    triples.map(
      ( t ) => { 
        val ( p, e, s ) = t
        PlaceInstance( p, e, s )
      }
    )
  }  

  def mgetWithSuspension(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : RetentionPolicy,
    cursor : RetentionPolicy
  )( ptn : Pattern )
  : Generator[Option[Resource],Unit,Unit] =
    Generator {
      rk : ( Option[Resource] => Unit @suspendable ) =>
        shift[Unit,Unit,Unit] {
          outerk : ( Unit => Unit ) =>
            reset[Unit,Unit] {
              val map = Left[Map[Place,Resource],Map[Place,List[RK]]]( channels )
              val meets = locations( map, ptn )

              if ( meets.isEmpty )  {
                val place = representative( ptn )
                //println( "did not find a resource, storing a continuation: " + rk )
                val bk = new BlockableContinuation( rk )
                registered( place ) =
                  (
                    registered.get( place ).getOrElse( Nil ) ++ List( bk )
                  )
                //rk( None )
                //println( "get suspending" )
                bk.blockForValue()
                //outerk()
              }
              else {
                for(
                  placeNRrscNSubst <- itergen[PlaceInstance](
                    meets
                  )
                ) {
                  val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
                  
                  //println( "found a resource: " + rsrc )                                
                  consume match {
                    case policy : RetainInCache => {
                      channels -= place
                    }
                    case _ => {
                      BasicLogService.tweet( "policy indicates not to remove from cache: " + place )
                    }
                  }

                  rk( s( rsrc ) )
                  //println( "get returning" )
                  outerk()
                  //shift { k : ( Unit => Unit ) => k() }
                }
              }
              //println( "get returning" )
              //outerk()
            }
        }
    }

  implicit val spaceLockTumbler : Option[Option[Resource] => Unit @suspendable] = None

  def mget(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : RetentionPolicy,
    keep : RetentionPolicy
  )( ptn : Pattern )( implicit spaceLockKey : Option[Option[Resource] => Unit @suspendable] )
  : Generator[Option[Resource],Unit,Unit] = {
    BasicLogService.tweet(
      (
	"MonadicTupleSpace : "
	+ "\nmethod : mget "
	+ "\nthis : " + this
	+ "\nchannels : " + channels
	+ "\nregistered : " + registered
	+ "\nconsume : " + consume
      )
    )
    Generator {
      rk : ( Option[Resource] => Unit @suspendable ) =>
        shift {
          outerk : ( Unit => Unit ) =>
            reset {
              val slk = spaceLockKey match {
                case None => Some( rk )
                case _ => spaceLockKey
              }

              //spaceLock.occupy( slk )
              spaceLock.occupy( ptn, slk )

              BasicLogService.tweet( "Reader occupying spaceLock on " + this + " for mget on " + ptn + "." )
              //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
              //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

              val map = Left[Map[Place,Resource],Map[Place,List[RK]]]( channels )
              val meets = locations( map, ptn )
              
              if ( meets.isEmpty ) {
                val place = representative( ptn )
                BasicLogService.tweet(
                  (
                    "did not find a resource, storing a continuation: " + rk
                    + "\nregistered continuation storage: " + registered 
                    + "\ntheWaiters: " + theWaiters
                    + "\ntheSubscriptions: " + theSubscriptions
                  )
                )
                
                keep match {
                  case policy : RetainInCache => {
                    registered( place ) =
                      registered.get( place ).getOrElse( Nil ) ++ List( rk )
                  }
                  case _ => {
                    BasicLogService.tweet( "policy indicates not to retain in cache: " + rk )
                  }
                }
                
                BasicLogService.tweet(
                  (
                    "stored a continuation: " + rk
                    + "\nregistered continuation storage: " + registered
                    + "\ntheWaiters: " + theWaiters
                    + "\ntheSubscriptions: " + theSubscriptions
                  )
                )
                                                
                // This code intentionally avoids departing lock in
                // RetainInStore cases
                keep match {
                  case storagePolicy : RetainInStore => {
                  }
                  case _ => {
                    BasicLogService.tweet(
                      "Reader departing spaceLock on "
                      + this + " for mget on " + ptn + "."
                    )
                    spaceLock.depart( ptn, slk )
                  }
                }
                
                rk( None )
              }
              else {
                for(
                  placeNRrscNSubst <- itergen[PlaceInstance](
                    meets
                  )
                ) {
                  val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
                  
                  BasicLogService.tweet( "found a resource: " + rsrc )
                  
                  consume match {
                    case policy : RetainInCache => {
                      channels -= place
                    }
                    case _ => {
                      BasicLogService.tweet( "policy indicates not to consume from cache: " + place )
                    }
                  }
                                    
                  // This code intentionally avoids departing lock in
                  // RetainInStore cases
                  keep match {
                    case storagePolicy : RetainInStore => {
                    }
                    case _ => {
                      BasicLogService.tweet(
                        "Reader departing spaceLock on "
                        + this + " for mget on " + ptn + "."
                      )
                      spaceLock.depart( ptn, slk )
                    }
                  }
                  
                  rk( s( rsrc ) )                 
                }
              }                         

              outerk()
            }
        }
    }
  }
  
  def get( ptn : Pattern ) =
    mget( theMeetingPlace, theWaiters, Cache, Cache )( ptn )
  def getS( ptn : Pattern ) =
    mgetWithSuspension( theMeetingPlace, theWaiters, Cache, Cache )( ptn )
  def fetch( ptn : Pattern ) =
    mget( theMeetingPlace, theWaiters, DoNotRetain, DoNotRetain )( ptn )
  def subscribe( ptn : Pattern ) =
    mget( theChannels, theSubscriptions, Cache, Cache )( ptn )  

  def putPlacesWithSuspension(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    ptn : Pattern,
    rsrc : Resource
  )
  : Generator[PlaceInstance,Unit,Unit] = {    
    Generator {
      k : ( PlaceInstance => Unit @suspendable ) => 
        // Are there outstanding waiters at this pattern?    
        BasicLogService.tweet(
          (
            ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
            + "in putPlaces\n"
            + "channels : " + channels + "\n"
            + "registered : " + registered + "\n"
            + "ptn : " + ptn + "\n"
            + "rsrc : " + rsrc + "\n"
            + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
          )
        )

      val map = Right[Map[Place,Resource],Map[Place,List[RK]]]( registered )
      val waitlist = locations( map, ptn )

        waitlist match {
          // Yes!
          case waiter :: waiters => {
            BasicLogService.tweet( "found waiters waiting for a value at " + ptn )
            val itr = waitlist.toList.iterator      
            while( itr.hasNext ) {
              k( itr.next )
            }
          }
          // No...
          case Nil => {
            // Store the rsrc at a representative of the ptn        
            channels( representative( ptn ) ) = rsrc
          }
        }
    }
  }

  def mputWithSuspension(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : Boolean
  )( ptn : Pattern, rsrc : Resource ) : Unit @suspendable = {    
    for( placeNRKsNSubst <- putPlacesWithSuspension( channels, registered, ptn, rsrc ) ) {
      val PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
      BasicLogService.tweet( "waiters waiting for a value at " + wtr + " : " + rks )
      rks match {
        case rk :: rrks => {    
          if ( consume ) {
            for( sk <- rks ) {
              sk match {
                case bk : BlockableContinuation => {
                  bk.unblockOnValue()
                }
                case _ => {
                }
              }

              spawn {
                sk( s( rsrc ) )
              }
            }
          }
          else {
            registered( wtr ) = rrks
            rk match {
                case bk : BlockableContinuation => {
                  bk.unblockOnValue()
                }
                case _ => {
                }
              }
            rk( s( rsrc ) )
          }
        }
        case Nil => {
          if ( ptn.isInstanceOf[Place] ) {
            channels( ptn.asInstanceOf[Place] ) = rsrc
          }
          else {
            channels( wtr ) = rsrc        
          }
        }
      }
    }
        
  }

  def putPlaces(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    ptn : Pattern,
    rsrc : Resource
  )
  : Generator[PlaceInstance,Unit,Unit] = {    
    Generator {
      k : ( PlaceInstance => Unit @suspendable ) => 
        // Are there outstanding waiters at this pattern?    
        BasicLogService.tweet(
          (
            ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
            + "in putPlaces\n"
            + "channels : " + channels + "\n"
            + "registered : " + registered + "\n"
            + "ptn : " + ptn + "\n"
            + "rsrc : " + rsrc + "\n"
            + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
          )
        )

      val map = Right[Map[Place,Resource],Map[Place,List[RK]]]( registered )
      val waitlist = locations( map, ptn )

        waitlist match {
          // Yes!
          case waiter :: waiters => {
            BasicLogService.tweet( "found waiters waiting for a value at " + ptn )
            val itr = waitlist.toList.iterator      
            while( itr.hasNext ) {
              k( itr.next )
            }
          }
          // No...
          case Nil => {
            // Store the rsrc at a representative of the ptn
            BasicLogService.tweet( "no waiters waiting for a value at " + ptn )
            channels( representative( ptn ) ) = rsrc

            BasicLogService.tweet( "Writer departing spaceLock on " + this + " for mput on " + ptn + "." )
            //spaceLock.depart( None )
            spaceLock.depart( ptn )
            //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
            //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
          }
        }
    }
  }

  def mput(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]],
    consume : Boolean
  )( ptn : Pattern, rsrc : Resource ) : Unit @suspendable = {        
    //spaceLock.occupy( None )
    spaceLock.occupy( ptn )
    BasicLogService.tweet( "Writer occupying spaceLock on " + this + " for mput on " + ptn + "." )
    //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
    //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

    for( placeNRKsNSubst <- putPlaces( channels, registered, ptn, rsrc ) ) {
      val PlaceInstance( wtr, Right( rks ), s ) = placeNRKsNSubst
      BasicLogService.tweet( "waiters waiting for a value at " + wtr + " : " + rks )
      rks match {
        case rk :: rrks => {    
          if ( consume ) {

            BasicLogService.tweet( "Writer departing spaceLock on " + this + " for mput on " + ptn + "." )
            //spaceLock.depart( None )
            spaceLock.depart( ptn )
            //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
            //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

            for( sk <- rks ) {
              spawn {
                sk( s( rsrc ) )
              }
            }
          }
          else {

            BasicLogService.tweet( "Writer departing spaceLock on " + this + " for mput on " + ptn + "." )
            //spaceLock.depart( None )
            spaceLock.depart( ptn )
            //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
            //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

            registered( wtr ) = rrks

            rk( s( rsrc ) )
          }
        }
        case Nil => {
          //channels( wtr ) = rsrc        
          //channels( ptn ) = rsrc        
          BasicLogService.tweet(
            (
              ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
              + "in mput with empty waitlist; about to put rsrc\n"
              + "channels : " + channels + "\n"
              + "registered : " + registered + "\n"
              + "ptn : " + ptn + "\n"
              + "rsrc : " + rsrc + "\n"
              + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
            )
          )
          if ( ptn.isInstanceOf[Place] ) {
            channels( ptn.asInstanceOf[Place] ) = rsrc
          }
          else {
            channels( wtr ) = rsrc
          }
          BasicLogService.tweet(
            (
              ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
              + "in mput with empty waitlist; having put rsrc\n"
              + "channels : " + channels + "\n"
              + "registered : " + registered + "\n"
              + "ptn : " + ptn + "\n"
              + "rsrc : " + rsrc + "\n"
              + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
            )
          )

          BasicLogService.tweet( "Writer departing spaceLock on " + this + " for mput on " + ptn + "." )
          //spaceLock.depart( None )
          spaceLock.depart( ptn )
          //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
          //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

        }
      }
    }        
    
  }

  def put( ptn : Pattern, rsrc : Resource ) =
    mput( theMeetingPlace, theWaiters, false )( ptn, rsrc )
  def putS( ptn : Pattern, rsrc : Resource ) =
    mputWithSuspension( theMeetingPlace, theWaiters, false )( ptn, rsrc )
  def publish( ptn : Pattern, rsrc : Resource ) =
    mput( theChannels, theSubscriptions, true )( ptn, rsrc )

  def mdelete(
    channels : Map[Place,Resource],
    registered : Map[Place,List[RK]]
  )( ptn : Pattern )(
    implicit spaceLockKey : Option[Option[Resource] => Unit @suspendable]
  ) : Generator[Option[PlaceInstance],Unit,Unit] = {
    Generator {
      rk : ( Option[PlaceInstance] => Unit @suspendable ) =>
        shift {
          outerk : ( Unit => Unit ) =>
            reset {
              val slk = spaceLockKey match {
                case None => Some( ( orsrc : Option[Resource] ) => rk( None ) )
                case _ => spaceLockKey
              }

              //spaceLock.occupy( slk )
              spaceLock.occupy( ptn, slk )
              BasicLogService.tweet( "Delete occupying spaceLock on " + this + " for delete on " + ptn + "." )
              //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
              //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

              val map = Left[Map[Place,Resource],Map[Place,List[RK]]]( channels )
              val meets = locations( map, ptn )
              
              if ( meets.isEmpty ) {
                BasicLogService.tweet( "Delete found no locations matching pattern: " + ptn )
                BasicLogService.tweet( "Delete departing spaceLock on " + this + " for delete on " + ptn + "." )
                //spaceLock.depart( slk )
                spaceLock.depart( ptn, slk )
                //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                
                //spaceLock.occupy( None )
                spaceLock.occupy( ptn )
                BasicLogService.tweet( "Delete occupying spaceLock on " + this + " for mdelete on " + ptn + "." )
                //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

                val map = Right[Map[Place,Resource],Map[Place,List[RK]]]( registered )
                val waitlist = locations( map, ptn )

                BasicLogService.tweet( "Delete departing spaceLock on " + this + " for mdelete on " + ptn + "." )
                //spaceLock.depart( None )
                spaceLock.depart( ptn )
                //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

                waitlist match {
                  // Yes!
                  case waiter :: waiters => {
                    BasicLogService.tweet( "found waiters waiting for a value at " + ptn )
                    val itr = waitlist.toList.iterator
                    // BUGBUG : lgm -- the compiler crashes if we
                    // attempt to use std pattern matching inside
                    // the while loop
                    var plcInst : PlaceInstance = null
                    while( itr.hasNext ) {
                      //val PlaceInstance( wtr, Right( rks ), s ) = itr.next
                      plcInst = itr.next
                      registered -= plcInst.place
                      rk( Some( plcInst ) )
                      //rk( None )
                    }
                  }
                  // No...
                  case Nil => {
                    // Store the rsrc at a representative of the ptn
                    BasicLogService.tweet( "no waiters waiting for a value at " + ptn )
                    rk( None )                              
                  }
                }                               
              }
              else {
                BasicLogService.tweet( "Delete found " + meets.length + " locations matching pattern: " + ptn )
                for(
                  placeNRrscNSubst <- itergen[PlaceInstance](
                    meets
                  )
                ) {
                  val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
                  
                  BasicLogService.tweet( "found a resource: " + rsrc )
                  
                  channels -= place                               

                  BasicLogService.tweet( "Delete departing spaceLock on " + this + " for delete on " + ptn + "." )
                  //spaceLock.depart( slk )
                  spaceLock.depart( ptn, slk )
                  //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                  //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

                  rk( Some( placeNRrscNSubst ) )
                  
                  //shift { k : ( Unit => Unit ) => k() }
                }
              }
              
              outerk()
            }
        }
    }
  }

  def delete( ptn : Pattern ) =
    mdelete( theMeetingPlace, theWaiters )( ptn )
  
}

package usage {
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

import java.util.regex.{Pattern => RegexPtn, Matcher => RegexMatcher}

object MonadicRegexTSpace
       extends MonadicTupleSpace[String,String,String]
       with WireTap
       with ConfigurationTrampoline
{

  override type Substitution = IdentitySubstitution

  override val theMeetingPlace = new HashMap[String,String]()
  override val theChannels = new HashMap[String,String]()
  override val theWaiters = new HashMap[String,List[RK]]()
  override val theSubscriptions = new HashMap[String,List[RK]]()

  override def tap [A] ( fact : A ) : Unit = {
    BasicLogService.reportage( fact )
  }

  override def configFileName : Option[String] = None
  override def configurationDefaults : ConfigurationDefaults = {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  def representative( ptn : String ) : String = {
    ptn
  }

  def fits( ptn : String, place : String ) : Boolean = {
    RegexPtn.matches( ptn, place ) || RegexPtn.matches( place, ptn )
  }

  def fitsK(
    ptn : String,
    place : String
  ) : Option[Substitution] = {
    //println( "in fitsK on " + this )
    if ( fits( ptn, place ) ) {
      Some( IdentitySubstitution() )
    }
    else {
      None
    }
  }
  
}


}
