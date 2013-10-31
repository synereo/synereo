// -*- mode: Scala;-*- 
// Filename:    MonadicTermSpace.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan 31 01:46:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

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

//import org.exist.storage.DBBroker

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

//import org.exist.util.serializer.SAXSerializer
//import org.exist.util.serializer.SerializerPool

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import javax.xml.transform.OutputKeys
import java.util.Properties
import java.net.URI
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter

trait DistributedAskTypes {
  trait Ask
  case object AGet extends Ask
  case object AFetch extends Ask
  case object ASubscribe extends Ask    
  
  // workaround due to bug in scala runtime
  type AskNum = Int
  val AGetNum : AskNum = 0
  val AFetchNum : AskNum = 1
  val ASubscribeNum : AskNum = 2
}

trait DistributedAskTypeScope {
  type DATypes <: DistributedAskTypes
  def protoAskTypes : DATypes
  val dAT : DATypes = protoAskTypes
}

trait MonadicSoloTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicTermTypeScope[Namespace,Var,Tag,Value] 
  with MonadicDTSMsgScope[Namespace,Var,Tag,Value]
  with DistributedAskTypeScope
{          

  trait MonadicTermStoreT
   extends MonadicTupleSpace[mTT.GetRequest,mTT.GetRequest,mTT.Resource] 
    with CnxnCtxtInjector[Namespace,Var,Tag]
    with CnxnUnificationCompositeTermQuery[Namespace,Var,Tag]
    with CnxnConversions[Namespace,Var,Tag]
    with WireTap
    with ConfigurationTrampoline
    with UUIDOps
    with Serializable
  {
    override def tap [A] ( fact : A ) : Unit = {
      BasicLogService.reportage( fact )
    }
    
    override lazy val theMeetingPlace =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override lazy val theChannels =
      new mTT.TMapR[Namespace,Var,Tag,Value]()
    override lazy val theWaiters =
      new TMapK[Namespace,Var,Tag,Value]()
    override lazy val theSubscriptions =
      new TMapK[Namespace,Var,Tag,Value]()

    class TMapK[Namespace,Var,Tag,Value]
    extends HashMap[mTT.GetRequest,List[RK]]

    case class PrologSubstitution( soln : LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]] )
         extends Function1[mTT.Resource,Option[mTT.Resource]] {
           override def apply( rsrc : mTT.Resource ) = {
             soln.isEmpty match {
               case true => Some( rsrc )
               case _ => Some( mTT.RBoundHM( Some( rsrc ), Some( soln ) ) )
             }
           }
         }

    override type Substitution = PrologSubstitution

    case class KeyKUnifiySpaceLock(
      @transient override val locker : HashMap[ModeSpaceLock[RK,mTT.GetRequest]#ModeType,Int],
      override val maxOccupancy : Int
    ) extends ModeSpaceLock[RK,mTT.GetRequest] 
         with CnxnUnificationTermQuery[Namespace,Var,Tag]
         with CnxnConversions[Namespace,Var,Tag] with UUIDOps {  
           override type ModeType = HigherStation[RK,mTT.GetRequest]
           override def makeMode( ptn : mTT.GetRequest, rk : Option[RK] ) : ModeType = {
             HigherStation[RK,mTT.GetRequest]( ptn, rk, true )
           }
           override def makeMode( ptn : mTT.GetRequest, rk : RK ) : ModeType = {
             HigherStation[RK,mTT.GetRequest]( ptn, Some( rk ), true )
           }
           override def makeMode( ptn : mTT.GetRequest ) : ModeType = {
             HigherStation[RK,mTT.GetRequest]( ptn, None, false )
           }
           def makeMode( ptn : mTT.GetRequest, rk : Option[RK], polarity : Boolean ) : ModeType = {
             HigherStation[RK,mTT.GetRequest]( ptn, rk, polarity )
           }
           def makeMode( ptn : mTT.GetRequest, rk : RK, polarity : Boolean ) : ModeType = {
             HigherStation[RK,mTT.GetRequest]( ptn, Some( rk ), polarity )
           }
           def makeMode( ptn : mTT.GetRequest, polarity : Boolean ) : ModeType = {
             HigherStation[RK,mTT.GetRequest]( ptn, None, polarity )
           }

           override def occupy( s : ModeType ) : Unit = { //synchronized {
             this.synchronized {
               while ( ! allowedIn( s ) ) wait()
             }
           }
           override def occupy( ptn : mTT.GetRequest ) : Unit = {
             occupy( makeMode( ptn, false ) )
           }
           override def occupy( ptn : mTT.GetRequest, rk : RK ) : Unit  = {
             occupy( makeMode( ptn, rk, true ) )
           }
           override def occupy( ptn : mTT.GetRequest, optRK : Option[RK] ) : Unit  = {
             occupy( makeMode( ptn, optRK, true ) )
           }
           
           // allowedIn( ( pattern, boolean ), m = [( pattern1, boolean1 ), ..., ( patternN, booleanN )] )
           // =
           // and( for( ( p, b ) <- m if unifies( pattern, p ) ) yield { b } )

           val predicateLock = new scala.concurrent.Lock()

           override def allowedIn( s : ModeType ) : Boolean = {
             val ptn = s.ptn          
             val polarity = s.polarity                                      
             def loop(
               pairs : List[( ModeSpaceLock[RK,mTT.GetRequest]#ModeType, Int )]
             ) : Boolean = {
               pairs match {
                 case Nil => true
                 case ( e, i ) :: rPairs => {
                   matchMap( e.ptn, ptn ) match {
                     case Some( _ ) => {
                       val hs : HigherStation[RK,mTT.GetRequest] =
                         e.asInstanceOf[HigherStation[RK,mTT.GetRequest]]
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
             predicateLock.acquire()
             val lockerList = locker.toList
             val pass = loop( locker.toList )
             BasicLogService.tweet(
               "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
               + "\nin method: KeyKUnifySpaceLock.allowedIn"
               + "\nthis : " + this
               + "\npass : " + pass
               + "\ns.ptn : " + s.ptn
               + "\ns.polarity : " + s.polarity
               + "\nlockerList : " + lockerList
               + "\n+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+"
             )
             if ( pass ) { locker += ( s -> 1 ) }
             predicateLock.release()
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

    // override def spaceLock : ModeSpaceLock[RK,mTT.GetRequest] = {
//       _spaceLock match {
//         case Some( sl ) => sl        
//         case None | null => {
//           val sl =
//             KeyKUnifiySpaceLock(
//               new HashMap[ModeSpaceLock[RK,mTT.GetRequest]#ModeType,Int](),
//               1
//             )
//           _spaceLock = Some( sl )
//           sl
//         }
//       }
//     }

    var _spaceLockKey : Option[String] = None
    override def spaceLock : ModeSpaceLock[RK,mTT.GetRequest] = {
      _spaceLockKey match {
        case Some( slk ) => {
          ExternalConditions.retrieveContent[ModeSpaceLock[RK,mTT.GetRequest]]( slk ) match {
            case Some( sl ) => sl
            case None => {
              val sl = 
                KeyKUnifiySpaceLock(
                  new HashMap[ModeSpaceLock[RK,mTT.GetRequest]#ModeType,Int](),
                  1
                )
              val slk =
                ExternalConditions.registerContentAsT[ModeSpaceLock[RK,mTT.GetRequest]](
                  sl
                )
              _spaceLockKey = Some( slk )
              sl
            }
          }
        }
        case None | null => {
          val sl = 
            KeyKUnifiySpaceLock(
              new HashMap[ModeSpaceLock[RK,mTT.GetRequest]#ModeType,Int](),
              1
            )
          val slk =
            ExternalConditions.registerContentAsT[ModeSpaceLock[RK,mTT.GetRequest]](
              sl
            )
          _spaceLockKey = Some( slk )
          sl
        }
      }
    }
    
    override def representative(
      ptn : mTT.GetRequest
    ) : mTT.GetRequest = {
      ptn
    }
    override def fits(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Boolean = {
      matchMap( ptn, place ) match {
        case Some( soln ) => {
          true
        }
        case None => {
          false
        }
      }
    }

    override def fitsK(
      ptn : mTT.GetRequest,
      place : mTT.GetRequest
    ) : Option[Substitution] = {
      val matchRslts = matchMap( ptn, place )
      matchRslts match {
        case Some( soln : LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]] ) => {
          Some( PrologSubstitution( soln ) )
        }
        case None => {
          None
        }
      }
    }

    def getGV( rsrc : mTT.Resource ) : Option[Value] = {
      rsrc match {
        case mTT.Ground( v ) => Some( v )
        case mTT.RBoundHM( Some( nrsrc ), _ ) => getGV( nrsrc )
        case _ => None
      }
    }    

    def asCursor(
      values : List[mTT.Resource]
    ) : Option[mTT.Resource] = {
      
      val ig : mTT.Generator[mTT.Resource, Unit, Unit]  = mTT.itergen[mTT.Resource]( values )
          
      // BUGBUG -- LGM need to return the Solution
      // Currently the PersistenceManifest has no access to the
      // unification machinery
      Some (
        mTT.RBoundHM(
          Some( mTT.Cursor( ig ) ),
          None
        )
      )
    }

    override def mget(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy
    )( ptn : mTT.GetRequest )( implicit spaceLockKey : Option[Option[mTT.Resource] => Unit @suspendable] )
    : Generator[Option[mTT.Resource],Unit,Unit] = {
      mget( channels, registered, consume, keep, false )( ptn )( spaceLockKey )
    }

    def mget(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy,
      cursor : Boolean
    )( ptn : mTT.GetRequest )( implicit spaceLockKey : Option[Option[mTT.Resource] => Unit @suspendable] )
    : Generator[Option[mTT.Resource],Unit,Unit] = {
      BasicLogService.tweet(
	(
	  "MonadicTermStoreT : "
	  + "\nmethod : mget "
	  + "\nthis : " + this
	  + "\nchannels : " + channels
	  + "\nregistered : " + registered
	  + "\nconsume : " + consume
	  + "\nkeep : " + keep
	)
      )
      Generator {
        rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
          shift {
            outerk : ( Unit => Unit ) =>
              reset {
                // if ( ! spaceLock.available ) {
                //              rk( None )
                //            }
                //            else {
                val slk = spaceLockKey match {
                  case None => Some( rk )
                  case _ => spaceLockKey
                }
                
                spaceLock.occupy( ptn, slk )
                
                BasicLogService.tweet(
                  "Reader occupying spaceLock on " + this + " for mget on " + ptn + "."
                )
                
                val map = Left[Map[mTT.GetRequest,mTT.Resource],Map[mTT.GetRequest,List[RK]]]( channels )
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
                  cursor match {
                    case true => {
                      val rsrcRslts : ListBuffer[mTT.Resource] = new ListBuffer[mTT.Resource]()
                      for( placeNRrscNSubst <- meets ) {
                        val PlaceInstance( place, Left( rsrc ), s ) = placeNRrscNSubst
                        
                        BasicLogService.tweet( "found a resource: " + rsrc )
                        
                        rsrcRslts += rsrc
                        
                        consume match {
                          case policy : RetainInCache => {
                            channels -= place
                          }
                          case _ => {
                            BasicLogService.tweet(
                              "policy indicates not to consume from cache: " + place
                            )
                          }
                        }
                        
                        //shift { k : ( Unit => Unit ) => k() }
                      }
                      
                      val rsrcCursor = asCursor( rsrcRslts.toList )
                                           
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
                      rk( rsrcCursor )
                    }
                    case false => {
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
                        
                        //shift { k : ( Unit => Unit ) => k() }
                      }
                    }
                  }
                  
                }                               

                outerk()
              }
          }
      }
    }
    
  }
  
  class MonadicTermStore(
  ) extends MonadicTermStoreT {    
    override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
    } 
  }

}

trait MonadicTermStoreScope[Namespace,Var,Tag,Value] 
extends MonadicSoloTermStoreScope[Namespace,Var,Tag,Value]  {

  /* abstract */ class MonadicGeneratorJunction(
    override val name : Moniker,
    override val acquaintances : Seq[Moniker],
    override val requests : ListBuffer[Msgs.JTSReq],
    override val responses : ListBuffer[Msgs.JTSRsp],
    override val nameSpace : Option[LinkedHashMap[Moniker,Socialite[Msgs.DReq,Msgs.DRsp]]],
    @transient override val traceMonitor : TraceMonitor[Msgs.DReq,Msgs.DRsp]
  )
  extends MonadicTermStore(
  ) with MonadicCollective
  with MonadicJSONAMQPDispatcher[Msgs.JTSReqOrRsp]
  with MonadicWireToTrgtConversion
  with MonadicGenerators {
    def this() = {
      this(
        MURI( new URI( "agent", "localhost", "/connect", "" ) ),
        Nil,
        new ListBuffer[Msgs.JTSReq](),
        new ListBuffer[Msgs.JTSRsp](),
        None,
        null
      )
    }

    override def acqQName( acqURI : Moniker ) : String = {
      acqURI.getPath.split( "/" ).last
    }    
    override def handleIncoming( dmsg : Msgs.JTSReqOrRsp ) : Unit = {
      throw new Exception( "handleIncoming not implemented" )
    }

    override def toString() : String = {
      name + " -> " + acquaintances
    }

    override type Wire = String
    override type Trgt = Msgs.JTSReqOrRsp

    @transient override lazy val agentTwistedPairs
    : Map[Moniker,SemiMonadicAgentJSONAMQPTwistedPair[String]] =
      meetNGreet( acquaintances )

    def forward(
      ask : dAT.Ask,
      hops : List[Moniker],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      BasicLogService.tweet(
        ( this + " in forwardGet with hops: " + hops )
      )

      // Dummy declarations to avoid a bug in the scala runtime
      val das = ask
      val dasClass = ask.getClass
      
      for(
        ( uri, jsndr ) <- agentTwistedPairs
        if !hops.contains( uri )
      ) {
        BasicLogService.tweet(
          ( this + " forwarding to " + uri )
        )
        val smajatp : SMAJATwistedPair =
          jsndr.asInstanceOf[SMAJATwistedPair]
        
        smajatp.send(
          ask match {
            case dAT.AGet => {
              Msgs.MDGetRequest[Namespace,Var,Tag,Value](
                path
              ).asInstanceOf[Msgs.DReq]
            }
            case dAT.AFetch => {
              Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
                path
              ).asInstanceOf[Msgs.DReq]
            }
            case dAT.ASubscribe => {
              Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
                path
              ).asInstanceOf[Msgs.DReq]
            }
          }
        )
      }
    }
    
    def forward(
      ask : dAT.AskNum,
      hops : List[Moniker],
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Unit = {

      BasicLogService.tweet(
        ( this + " in forwardGet with hops: " + hops )
      )

      for(
        ( uri, jsndr ) <- agentTwistedPairs
        if !hops.contains( uri )
      ) {
        BasicLogService.tweet(
          ( this + " forwarding to " + uri )
        )
        val smajatp : SMAJATwistedPair =
          jsndr.asInstanceOf[SMAJATwistedPair]
        
        smajatp.send(
          ask match {
            case dAT.AGetNum => {
              Msgs.MDGetRequest[Namespace,Var,Tag,Value](
                path
              ).asInstanceOf[Msgs.DReq]
            }
            case dAT.AFetchNum => {
              Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
                path
              ).asInstanceOf[Msgs.DReq]
            }
            case dAT.ASubscribeNum => {
              Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
                path
              ).asInstanceOf[Msgs.DReq]
            }
          }
        )
      }
    }
  }

  /*
  class DistributedMonadicGeneratorJunction(
    override val name : Moniker,
    override val acquaintances : Seq[Moniker]
  ) extends MonadicGeneratorJunction(
    name,
    acquaintances,
    new ListBuffer[Msgs.JTSReq](),
    new ListBuffer[Msgs.JTSRsp](),
    Some( new LinkedHashMap[Moniker,Socialite[Msgs.DReq,Msgs.DRsp]]() ),
    AnAMQPTraceMonitor
  ) with QueueNameVender {
    def this() = {
      this(
        MURI( new URI( "agent", "localhost", "/connect", "" ) ),
        Nil
      )
    }
    override def acqQName( acqURI : Moniker ) : String = {
      acqURI.getPath.split( "/" ).last
    }
    def sendRsp(
      atp : SemiMonadicAgentJSONAMQPTwistedPair[String],
      dreq : Msgs.DReq, 
      oGv : Option[Value]
    ) = {
      val smajatp : SMAJATwistedPair =
        atp.asInstanceOf[SMAJATwistedPair]
      
      smajatp.send(
        dreq match {
          case Msgs.MDGetRequest( path ) => {
            oGv match {
              case Some( gv ) => {
                Msgs.MDGetResponse[Namespace,Var,Tag,Value](
                  path,
                  gv
                )
              }
              case None => {
                throw new Exception( "get must take value" )
              }
            }
          }
          case Msgs.MDFetchRequest( path ) => {
            oGv match {
              case Some( gv ) => {
                Msgs.MDFetchResponse[Namespace,Var,Tag,Value](
                  path,
                  gv
                )
              }
              case None => {
                throw new Exception( "fetch must take value" )
              }
            }
          }
          case Msgs.MDSubscribeRequest( path ) => {
            oGv match {
              case Some( gv ) => {
                Msgs.MDSubscribeResponse[Namespace,Var,Tag,Value](
                  path,
                  gv
                )
              }
              case None => {
                throw new Exception( "subscribe must take value" )
              }
            }
          }
          case Msgs.MDPutRequest( path, _ ) => {
            Msgs.MDPutResponse[Namespace,Var,Tag,Value](
              path
            )
          }
          case Msgs.MDPublishRequest( path, _ ) => {
            Msgs.MDPublishResponse[Namespace,Var,Tag,Value](
              path
            )
          }
        }
      )
    }

    def handleValue(
      dreq : Msgs.DReq,
      oV : Option[mTT.Resource],
      msrc : Moniker
    ) : Unit = {
      for(
        atp <- agentTwistedPairs.get( msrc );
        value <- oV
      ) {       

        value match {
          case mTT.RBoundHM(
            Some( mTT.Ground( gv ) ),
            Some( soln ) 
          ) => {
            BasicLogService.tweet(
              (
                this + " sending value " + oV + " back "
              )
            )
            
            sendRsp( atp, dreq, Some( gv ) )
              
          }

          case mTT.RBoundHM(
            Some( mTT.Ground( gv ) ),
            None 
          ) => {
            BasicLogService.tweet(
              (
                this + " sending value " + oV + " back "
              )
            )
            
            sendRsp( atp, dreq, Some( gv ) )
              
          }

          case mTT.Ground( gv ) => {
            BasicLogService.tweet(
              (
                this + " sending value " + oV + " back "
              )
            )
            
            sendRsp( atp, dreq, Some( gv ) )

          }
          case _ => {
            BasicLogService.tweet(
              (
                this 
                + " not sending composite value " + oV
                + " back "
              )
            )
          }
        }
      }
      oV
    }

    def handleRequest( dreq : Msgs.JTSReq ) : Unit = {      
      val JustifiedRequest( 
        msgId, mtrgt, msrc, lbl, body, _
      ) = dreq

      BasicLogService.tweet( this + "handling : " + dreq )

      body match {
        case dgreq@Msgs.MDGetRequest( path ) => {         
          BasicLogService.tweet(
            ( this + "getting locally for location : " + path )
          )
          reset {
            for( v <- get( List( msrc ) )( false )( path ) ) {
              BasicLogService.tweet(
                (
                  this 
                  + " returning from local get for location : "
                  + path
                  + "\nwith value : " + v
                )
              )
              handleValue( dgreq, v, msrc )
            }
          }
        }
        
        case dfreq@Msgs.MDFetchRequest( path ) => {
          BasicLogService.tweet(
            ( this + "fetching locally for location : " + path )
          )
          reset {
            for( v <- fetch( List( msrc ) )( false )( path ) ) {
              BasicLogService.tweet(
                (
                  this 
                  + " returning from local fetch for location : "
                  + path
                  + "\nwith value : " + v
                )
              )
              handleValue( dfreq, v, msrc )
            }
          }
        }

        case dsreq@Msgs.MDSubscribeRequest( path ) => {
          BasicLogService.tweet(
            ( this + "fetching locally for location : " + path )
          )
          reset {
            for( v <- subscribe( List( msrc ) )( path ) ) {
              BasicLogService.tweet(
                (
                  this 
                  + " returning from local subscribe for location : "
                  + path
                  + "\nwith value : " + v
                )
              )
              handleValue( dsreq, v, msrc )
            }
          }
        }
        
        case dpreq@Msgs.MDPutRequest( path, value ) => {        
          reset { put( path, mTT.Ground( value ) ) }
          for( atp <- agentTwistedPairs.get( msrc ) ) {
            sendRsp( atp, dpreq, None )
          }
        }
        case dpbreq@Msgs.MDPublishRequest( path, value ) => {   
          reset { publish( path, mTT.Ground( value ) ) }
          for( atp <- agentTwistedPairs.get( msrc ) ) {
            sendRsp( atp, dpbreq, None )
          }
        }
      }
    }
    
    def handleResponse( drsp : Msgs.JTSRsp ) : Unit = {      
      val JustifiedResponse( 
          msgId, mtrgt, msrc, lbl, body, _
      ) = drsp

      body match {
        case Msgs.MDGetResponse( path, value ) => {
          reset { put( path, mTT.Ground( value ) ) }
        }
        case Msgs.MDFetchResponse( path, value ) => {
          reset { put( path, mTT.Ground( value ) ) }
        }
        case Msgs.MDSubscribeResponse( path, value ) => {
          reset { publish( path, mTT.Ground( value ) ) }
        }
        case dput : Msgs.MDPutResponse[Namespace,Var,Tag,Value] => {    
        }
        case dpub : Msgs.MDPublishResponse[Namespace,Var,Tag,Value] => {        
        }
        case _ => {
          BasicLogService.tweet(
            (
              this 
              + " handling unexpected message : " + body
            )
          )
        }
      }
    }
    
    override def handleIncoming( dmsg : Msgs.JTSReqOrRsp ) : Unit = {
      dmsg match {
        case Left(
          dreq@JustifiedRequest( 
            msgId, mtrgt, msrc, lbl, body, _
          )
        ) => {
          BasicLogService.tweet(
            (
              this + " handling : " + dmsg
              + " from " + msrc
              + " on behalf of " + mtrgt
            )
          )
          handleRequest( dreq )
        }
        case Right(
          drsp@JustifiedResponse( 
            msgId, mtrgt, msrc, lbl, body, _
          )
        ) => {
          BasicLogService.tweet(
            (
              this + " handling : " + dmsg
              + " from " + msrc
              + " on behalf of " + mtrgt
            )
          )
          handleResponse( drsp )
        }
      }
    }

    def mget( ask : dAT.Ask, hops : List[Moniker] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy,
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      Generator {
        rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
          shift {
            outerk : ( Unit => Unit ) =>
              reset {
                for(
                  oV <- mget( channels, registered, consume, keep, cursor )( path ) 
                ) {
                  oV match {
                    case None => {
                      forward( ask, hops, path )
                      rk( oV )
                    }
                    case _ => rk( oV )
                  }
                }
              }
          }
      }
    }

    def mget( ask : dAT.AskNum, hops : List[Moniker] )(
      channels : Map[mTT.GetRequest,mTT.Resource],
      registered : Map[mTT.GetRequest,List[RK]],
      consume : RetentionPolicy,
      keep : RetentionPolicy,
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      Generator {
        rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
          shift {
            outerk : ( Unit => Unit ) =>
              reset {
                for(
                  oV <- mget( channels, registered, consume, keep, cursor )( path ) 
                ) {
                  oV match {
                    case None => {
                      //BasicLogService.tweet( ">>>>> forwarding..." )
                      forward( ask, hops, path )
                      rk( oV )
                    }
                    case _ => rk( oV )
                  }
                }
              }
          }
      }
    }
  
    def get( hops : List[Moniker] )(
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {              
      mget( dAT.AGetNum, hops )(
        theMeetingPlace, theWaiters, Cache, Cache, cursor
      )( path )    
    }

    def get(
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {
      get( Nil )( cursor )( path )
    }    

    override def get(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      get( Nil )( false )( path )    
    }

    def getValueWithSuspension(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] = {
      var suspension : Option[Unit => Unit] = None
      def suspend : Unit @suspendable = {
        shift {
          ( k : Unit => Unit ) => {
            suspension = Some( k )
          }
        }
      }
      Generator {
        k : ( Value => Unit @suspendable ) =>
          for(
            orsrc <- get( path )
          ) {
            orsrc match {
              case Some( rsrc ) => {
                getGV( rsrc ) match {
                  case Some( gv ) => k( gv )
                  case None =>
                }
              }
              case None => {
                suspend;
              }
            };
          }
      }
    }

    def getValue(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] =
      Generator {
        k : ( Value => Unit @suspendable ) =>
          for(
            orsrc <- get( path )
          ) {
            orsrc match {
              case Some( rsrc ) => {
                getGV( rsrc ) match {
                  case Some( gv ) => k( gv )
                  case None =>
                }
              }
              case None => 
            };
          }
      }

    def fetch( hops : List[Moniker] )(
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {              
      mget( dAT.AFetchNum, hops )(
        theMeetingPlace, theWaiters, DoNotRetain, DoNotRetain, cursor
      )( path )    
    }

    def fetch(
      cursor : Boolean
    )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {
      get( Nil )( cursor )( path )
    }

    override def fetch(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      fetch( Nil )( false )( path )    
    }

    def fetchValue(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] = 
      Generator {
        k : ( Value => Unit @suspendable ) =>
          for(
            orsrc <- fetch( path )
          ) {
            orsrc match {
              case Some( rsrc ) => {
                getGV( rsrc ) match {
                  case Some( gv ) => k( gv )
                  case None =>
                }
              }
              case None => 
            };
          }
      }

    def subscribe( hops : List[Moniker] )(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      mget( dAT.ASubscribeNum, hops )(
        theChannels, theSubscriptions, Cache, Cache, false
      )( path )    
    }

    override def subscribe(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    )
    : Generator[Option[mTT.Resource],Unit,Unit] = {        
      subscribe( Nil )( path )    
    }
    
    def subscribeValue(
      path : CnxnCtxtLabel[Namespace,Var,Tag]
    ) : Generator[Value,Unit,Unit] = 
      Generator {
        k : ( Value => Unit @suspendable ) =>
          for(
            orsrc <- subscribe( path )
          ) {
            orsrc match {
              case Some( rsrc ) => {
                getGV( rsrc ) match {
                  case Some( gv ) => k( gv )
                  case None =>
                }
              }
              case None => 
            };
          }
      }
  }
  */
   
}

package usage {
/* ------------------------------------------------------------------
 * Mostly self-contained object to support unit testing
 * ------------------------------------------------------------------ */ 

/*
object MonadicTS
 extends MonadicTermStoreScope[String,String,String,String] 
  with UUIDOps {
    import SpecialKURIDefaults._
    import CnxnLeafAndBranch._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes with Serializable
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT
    
    lazy val Mona = new MonadicTermStore()
    def ptToPt( a : String, b : String )  =
      new DistributedMonadicGeneratorJunction( a, List( b ) )    
    def loopBack() = {
      ptToPt( "localhost", "localhost" )
    }
    
    type MsgTypes = DTSMSH[String,String,String,String]   
    
    val protoDreqUUID = getUUID()
    val protoDrspUUID = getUUID()    
    
    object MonadicDMsgs extends MsgTypes with Serializable {
      
      override def protoDreq : DReq = MDGetRequest( aLabel )
      override def protoDrsp : DRsp = MDGetResponse( aLabel, aLabel.toString )
      override def protoJtsreq : JTSReq =
        JustifiedRequest(
          protoDreqUUID,
          new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
          new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
          getUUID(),
          protoDreq,
          None
        )
      override def protoJtsrsp : JTSRsp = 
        JustifiedResponse(
          protoDreqUUID,
          new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
          new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
          getUUID(),
          protoDrsp,
          None
        )
      override def protoJtsreqorrsp : JTSReqOrRsp =
        Left( protoJtsreq )
    }
    
    override def protoMsgs : MsgTypes = MonadicDMsgs
  }
  */

}
