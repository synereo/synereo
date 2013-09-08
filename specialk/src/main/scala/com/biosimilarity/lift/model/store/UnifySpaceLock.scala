// -*- mode: Scala;-*- 
// Filename:    UnifySpaceLock.scala 
// Authors:     lgm                                                    
// Creation:    Mon Aug 26 15:41:04 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store
import com.biosimilarity.lift.lib._

import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.util.continuations._ 
import scala.xml._
import scala.collection.MapProxy
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import org.prolog4j._

class KeyKUnifySpaceLock[Namespace,Var,Tag,RK](
  @transient override val locker : HashMap[ModeSpaceLock[RK,CnxnCtxtLabel[Namespace,Var,Tag]]#ModeType,Int],
  override val maxOccupancy : Int
) extends ModeSpaceLock[RK,CnxnCtxtLabel[Namespace,Var,Tag]] 
     with CnxnUnificationTermQuery[Namespace,Var,Tag]
     with CnxnConversions[Namespace,Var,Tag] with UUIDOps {  
       override type ModeType = HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]
       override def makeMode( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rk : Option[RK] ) : ModeType = {
         HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]( ptn, rk, true )
       }
       override def makeMode( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rk : RK ) : ModeType = {
         HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]( ptn, Some( rk ), true )
       }
       override def makeMode( ptn : CnxnCtxtLabel[Namespace,Var,Tag] ) : ModeType = {
         HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]( ptn, None, false )
       }
       def makeMode( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rk : Option[RK], polarity : Boolean ) : ModeType = {
         HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]( ptn, rk, polarity )
       }
       def makeMode( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rk : RK, polarity : Boolean ) : ModeType = {
         HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]( ptn, Some( rk ), polarity )
       }
       def makeMode( ptn : CnxnCtxtLabel[Namespace,Var,Tag], polarity : Boolean ) : ModeType = {
         HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]( ptn, None, polarity )
       }
       
       override def occupy( s : ModeType ) : Unit = synchronized {
         while ( ! allowedIn( s ) ) wait()
       }
       override def occupy( ptn : CnxnCtxtLabel[Namespace,Var,Tag] ) : Unit = {
         occupy( makeMode( ptn, false ) )
       }
       override def occupy( ptn : CnxnCtxtLabel[Namespace,Var,Tag], rk : RK ) : Unit  = {
         occupy( makeMode( ptn, rk, true ) )
       }
       override def occupy( ptn : CnxnCtxtLabel[Namespace,Var,Tag], optRK : Option[RK] ) : Unit  = {
         occupy( makeMode( ptn, optRK, true ) )
       }
       
       // allowedIn( ( pattern, boolean ), m = [( pattern1, boolean1 ), ..., ( patternN, booleanN )] )
       // =
       // and( for( ( p, b ) <- m if unifies( pattern, p ) ) yield { b } )
       
       override def allowedIn( s : ModeType ) : Boolean = {
         val ptn = s.ptn          
         val polarity = s.polarity
         
         def loop(
           pairs : List[( ModeSpaceLock[RK,CnxnCtxtLabel[Namespace,Var,Tag]]#ModeType, Int )]
         ) : Boolean = {
           pairs match {
             case Nil => true
             case ( e, i ) :: rPairs => {
               matchMap( e.ptn, ptn ) match {
                 case Some( _ ) => {
                   val hs : HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]] =
                     e.asInstanceOf[HigherStation[RK,CnxnCtxtLabel[Namespace,Var,Tag]]]
                   if ( hs.polarity && polarity ) {
                     loop( rPairs )
                   }
                   else false
                 }
                 case None => {
                   loop( rPairs )
                 }
               }
             }
           }             
         }
         
         val lockerList = locker.toList
         val pass = loop( locker.toList )
         BasicLogService.tweet(
           "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
           + "\nin method: allowedIn"
           + "\nthis : " + this
           + "\npass : " + pass
           + "\ns.ptn : " + s.ptn
           + "\ns.polarity : " + s.polarity
           + "\nlockerList : " + lockerList
           + "\n+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
         )
         if ( pass ) { locker += ( s -> 1 ) }
         pass
       }
       override def leave( s : ModeType ) : Unit = {
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
     }

object KeyKUnifySpaceLock {
  import scala.reflect.runtime.universe._
  def apply [Namespace,Var,Tag,RK] (
    locker : HashMap[ModeSpaceLock[RK,CnxnCtxtLabel[Namespace,Var,Tag]]#ModeType,Int],
    maxOccupancy : Int
  ) : KeyKUnifySpaceLock[Namespace,Var,Tag,RK] = {
    new KeyKUnifySpaceLock( locker, maxOccupancy )
  }
  def apply [Namespace : TypeTag, Var : TypeTag, Tag : TypeTag, RK : TypeTag] (
    weakMap : ExternalConditionsT,
    locker : HashMap[ModeSpaceLock[RK,CnxnCtxtLabel[Namespace,Var,Tag]]#ModeType,Int],
    maxOccupancy : Int
  ) : ( String, KeyKUnifySpaceLock[Namespace,Var,Tag,RK] ) = {
    val lock = new KeyKUnifySpaceLock( locker, maxOccupancy )    
    val tag = weakMap.registerContentAsT[KeyKUnifySpaceLock[Namespace,Var,Tag,RK]]( lock )
    ( tag, lock )
  }
  def unapply [Namespace,Var,Tag,RK] ( 
    kkusl : KeyKUnifySpaceLock[Namespace,Var,Tag,RK]
  ) : Option[(HashMap[ModeSpaceLock[RK,CnxnCtxtLabel[Namespace,Var,Tag]]#ModeType,Int], Int)] = {
    Some( ( kkusl.locker, kkusl.maxOccupancy ) )
  }
}

package unify.space.lock.usage {
  import fuzzer._  
  object TheFuzz extends FuzzyTermStreams
    with CnxnString[String,String,String]
    with FuzzyTerms with FuzzyStreams {
  }
  object ExerciseSpaceLock {
    import TheFuzz._
    type RK = Option[CnxnCtxtLabel[String,String,String]] => Unit @suspendable  
    case class Warlock( 
      override val maxOccupancy : Int
    ) extends KeyKUnifySpaceLock[String,String,String,RK](
      new HashMap[ModeSpaceLock[RK,CnxnCtxtLabel[String,String,String]]#ModeType,Int](),
      maxOccupancy
    )
    object Harry extends Warlock( 1 )
    object Ron extends Warlock( 1 )
    object Hermione extends Warlock( 2 )
    object Dumbledore extends Warlock( 100 )

    def populateReaderLocks(
      numLabels : Int = 10,
      maxWaitTime : Int = 1000,
      numThreadsPerLabel : Int = 1,
      lock : KeyKUnifySpaceLock[String,String,String,RK] = Harry,
      rlblStrm : Stream[CnxnCtxtLabel[String,String,String]] = randomLabelStream,
      rndm : scala.util.Random = new scala.util.Random()
    ) : Unit = {      
      for( lbl <- rlblStrm.take( numLabels ) ) {
        for( i <- 1 to numThreadsPerLabel ) {
          val t = 
            new Thread {
              override def run() = {
                val sleepTime : Int = rndm.nextInt( maxWaitTime ) + 1
                BasicLogService.tweet( ">R>R>R>R>R>R>R>R>R>R>R>R>R>R>R>R" )
                BasicLogService.tweet( "     reader occupying lock      " )
                BasicLogService.tweet( ">R>R>R>R>R>R>R>R>R>R>R>R>R>R>R>R" )
                lock.occupy( lbl );
                Thread.sleep( sleepTime )
                lock.depart( lbl )
                BasicLogService.tweet( "<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R" )
                BasicLogService.tweet( "     reader departing lock      " )
                BasicLogService.tweet( "<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R" )
              }
            }
          t.run()
        }
      }
    }

    def populateWriterLocks(
      numLabels : Int = 10,
      maxWaitTime : Int = 1000,
      numThreadsPerLabel : Int = 1,
      lock : KeyKUnifySpaceLock[String,String,String,RK] = Harry,
      rlblStrm : Stream[CnxnCtxtLabel[String,String,String]] = randomLabelStream,
      rndm : scala.util.Random = new scala.util.Random()
    ) : Unit = {      
      for( lbl <- rlblStrm.take( numLabels ) ) {
        for( i <- 1 to numThreadsPerLabel ) {
          val t =
            new Thread {
              override def run() = {
                val sleepTime : Int = rndm.nextInt( maxWaitTime ) + 1
                BasicLogService.tweet( ">W>W>W>W>W>W>W>W>W>W>W>W>W>W>W>W" )
                BasicLogService.tweet( "     writer occupying lock      " )
                BasicLogService.tweet( ">W>W>W>W>W>W>W>W>W>W>W>W>W>W>W>W" )
                lock.occupy( lbl );
                Thread.sleep( sleepTime )
                lock.depart( lbl )
                BasicLogService.tweet( "<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W" )
                BasicLogService.tweet( "     writer departing lock      " )
                BasicLogService.tweet( "<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W" )
              }
            }
          t.run()
        }
      }
    }

    def populateReaderWriterLocks(
      numLabels : Int = 10,
      maxWaitTime : Int = 1000,
      numThreadsPerLabel : Int = 1,
      lock : KeyKUnifySpaceLock[String,String,String,RK] = Harry,
      rlblStrm : Stream[CnxnCtxtLabel[String,String,String]] = randomLabelStream,
      rndm : scala.util.Random = new scala.util.Random()
    ) = {
      new Thread {
        override def run() = {
          populateReaderLocks(
            numLabels,
            maxWaitTime,
            numThreadsPerLabel,
            lock,
            rlblStrm,
            rndm
          )
        }
      }.run()
      new Thread {
        override def run() = {
          populateWriterLocks(
            numLabels,
            maxWaitTime,
            numThreadsPerLabel,
            lock,
            rlblStrm,
            rndm
          )
        }
      }.run()
    }

    def populateReaderWriterLocksInterleaved(
      numLabels : Int = 10,
      maxWaitTime : Int = 1000,
      numThreadsPerLabel : Int = 4,
      lock : KeyKUnifySpaceLock[String,String,String,RK] = Harry,
      rlblStrm : Stream[CnxnCtxtLabel[String,String,String]] = randomLabelStream,
      rndm : scala.util.Random = new scala.util.Random()
    ) : Unit = {      
      for( lbl <- rlblStrm.take( numLabels ) ) {
        for( i <- 1 to numThreadsPerLabel ) {
          val flip = rndm.nextInt( 2 )
          val t =
            if ( ( flip % 2 ) == 0 ) {            
              new Thread {
                override def run() = {
                  val sleepTime : Int = rndm.nextInt( maxWaitTime ) + 1
                  BasicLogService.tweet( ">R>R>R>R>R>R>R>R>R>R>R>R>R>R>R>R" )
                  BasicLogService.tweet( "     reader occupying lock      " )
                  BasicLogService.tweet( ">R>R>R>R>R>R>R>R>R>R>R>R>R>R>R>R" )
                  lock.occupy( lbl );
                  Thread.sleep( sleepTime )
                  lock.depart( lbl )
                  BasicLogService.tweet( "<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R" )
                  BasicLogService.tweet( "     reader departing lock      " )
                  BasicLogService.tweet( "<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R<R" )
                }
              }
            }
          else {
            new Thread {
                override def run() = {
                  val sleepTime : Int = rndm.nextInt( maxWaitTime ) + 1
                  BasicLogService.tweet( ">W>W>W>W>W>W>W>W>W>W>W>W>W>W>W>W" )
                  BasicLogService.tweet( "     writer occupying lock      " )
                  BasicLogService.tweet( ">W>W>W>W>W>W>W>W>W>W>W>W>W>W>W>W" )
                  lock.occupy( lbl );
                  Thread.sleep( sleepTime )
                  lock.depart( lbl )
                  BasicLogService.tweet( "<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W" )
                  BasicLogService.tweet( "     writer departing lock      " )
                  BasicLogService.tweet( "<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W<W" )
                }
              }
          }
          t.run()
        }
      }
    }
  }
}
