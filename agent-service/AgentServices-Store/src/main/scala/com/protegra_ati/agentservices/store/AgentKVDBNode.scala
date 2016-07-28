// -*- mode: Scala;-*- 
// Filename:    AgentKVDBNode.scala 
// Authors:     lgm                                                    
// Creation:    Mon Apr 30 20:37:26 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.store

import com.protegra_ati.agentservices.store.extensions.URIExtensions._
//import com.protegra_ati.agentservices.store.extensions.URMExtensions._
import com.protegra_ati.agentservices.store.extensions.MonikerExtensions._

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._
import com.biosimilarity.lift.lib.concurrent._
import com.biosimilarity.lift.lib.concurrent.cpsops._
import scala.xml._
import scala.collection.mutable.Map
import scala.collection.mutable.MapProxy
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import com.rabbitmq.client._

import org.prolog4j._

import org.xmldb.api.base.{ Resource => XmlDbRrsc, _}
import org.xmldb.api.modules._
import org.xmldb.api._

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import biz.source_code.base64Coder.Base64Coder

import javax.xml.transform.OutputKeys

import java.util.UUID
import java.net.URI
import java.util.Properties
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

trait AgentKVDBNodeScope[Namespace,Var,Tag,Value] 
extends PersistedMonadicKVDBNodeScope[Namespace,Var,Tag,Value]
//with CnxnDTSMsgScope[Namespace,Var,Tag,Value]
with AgentCnxnTypeScope {
  trait AgentPersistenceScope extends PersistenceScope {
    class BaseAgentKVDB[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse, +KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]](
      override val name : Moniker      
    ) extends BasePersistedMonadicKVDB[ReqBody,RspBody,KVDBNode](
      name
    ) {
      def mget( cnxn : acT.AgentCnxn )(
        persist : Option[PersistenceManifest],
        ask : dAT.AskNum,
        hops : List[Moniker]
      )(
        channels : Map[mTT.GetRequest,mTT.Resource],
        registered : Map[mTT.GetRequest,List[RK]],
        consume : RetentionPolicy,
        keep : RetentionPolicy,
        cursor : Boolean,
        collName : Option[String]
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        BasicLogService.tweet(
          (
            "PersistedMonadicKVDBNode : "
            + "\nmethod : mget "
            + "\nthis : " + this
            + "\ncnxn : " + cnxn
            + "\nchannels : " + channels
            + "\nregistered : " + registered
            + "\nconsume : " + consume
            + "\nkeep : " + keep
            + "\ncursor : " + cursor
            + "\ncollName : " + collName
            + "\npath : " + path
            + "\n---------------------------------------"
            + "\nchecking if this is a cache or a node: "
            + "cache" + "\n"
          )
        )

        def storeKQuery( xmlCollName : String, pd : PersistenceManifest )(
            path : CnxnCtxtLabel[Namespace,Var,Tag],
            keep : RetentionPolicy,
            rk : ( Option[mTT.Resource] => Unit @suspendable )
          ) : Unit @suspendable = {

          keep match {
            case policy: RetainInStore => {
            } // Need to store the
            // continuation on the tail of
            // the continuation entry
            val oKQry = kquery(xmlCollName, path)
            oKQry match {
              case None => {
                throw new Exception(
                  "failed to compile a continuation query"
                )
              }
              case Some(kqry) => {
                val krslts = executeWithResults(xmlCollName, kqry)

                // This is the easy case!
                // There are no locations
                // matching the pattern with
                // stored continuations
                krslts match {
                  case Nil => {
                    putKInStore(
                      persist,
                      path,
                      mTT.Continuation(List(rk)),
                      collName,
                      false
                    )
                  }
                  case _ => {
                    // A more subtle
                    // case. Do we store
                    // the continutation on
                    // each match?
                    // Answer: Yes!
                    for ( krslt <- itergen[ Elem ](krslts) ) {
                      BasicLogService.tweet("retrieved " + krslt.toString)
                      val ekrsrc = pd.asResource(path, krslt)

                      ekrsrc.stuff match {
                        case Right(ks) => {
                          BasicLogService.tweet("removing from store " + krslt)
                          removeFromStore(
                            persist,
                            krslt,
                            collName
                          )
                          putKInStore(
                            persist,
                            path,
                            mTT.Continuation(ks ++ List(rk)),
                            collName,
                            false
                          )
                        }
                        case _ => {
                          throw new Exception(
                            "Non-continuation resource stored in kRecord" + ekrsrc
                          )
                        }
                      }
                    }
                  }
                }
              }
            }
            case _ => {
              BasicLogService.tweet("policy indicates not to store kQuery: " + path)
            }
          }
        }

        Generator {
          rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
            shift {
              outerk : ( Unit => Unit ) =>
                reset {
                  val results = mget( channels, registered, consume, keep )( path )( Some( rk ) )
                  var processed = false
                  
                  for(
                    oV <-results
                  ) {
                    oV match {
                      case None => {
                        BasicLogService.tweet( 
                          (
                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                            + "mgetting " + path + ".\n"
                            + "on " + this + "did not find a result in memory cache.\n"
                            + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                          )
                        )
                        persist match {
                          case None => {
                            BasicLogService.tweet( 
                              (
                                "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                + "mgetting " + path + ".\n"
                                + "on " + this + "without persistence manifest.\n"
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                              )
                            )
                            BasicLogService.tweet( "Reader departing spaceLock AgentKVDBNode Version 1 on " + this + " for mget on " + path + "." )
                            //spaceLock.depart( Some( rk ) )
                            spaceLock.depart( path, rk )
                            //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                            //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

                            rk(oV)
                          }
                          case Some(pd) => {                        
                            val xmlCollName =
                              collName.getOrElse(
                                storeUnitStr.getOrElse(
                                  bail()
                                )
                              )

                            BasicLogService.tweet( 
                              (
                                "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                + "mgetting " + path + ".\n"
                                + "on " + this.name + " with persistence manifest.\n"
                                + "accessing db : " + pd.db + "\n"
                                + "collection : " + xmlCollName
                                + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                              )
                            )
                            
                            // Defensively check that db is actually
                            // available

                            BasicLogService.tweet(
                              "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                              + "checking if db exists"
                              + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                            )
                            
                            checkIfDBExistsAndCreateIfNot(xmlCollName, true) match {
                              case true => {
                                BasicLogService.tweet( 
                                  (
                                    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                    + "mgetting " + path + ".\n"
                                    + "on " + this + "\n"
                                    + pd.db + " exists\n"
                                    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                  )
                                )
                                val oQry = query(xmlCollName, path)
                                
                                oQry match {
                                  case None => {
                                    BasicLogService.tweet( 
                                      (
                                        "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                        + "mgetting " + path + ".\n"
                                        + "on " + this + " from db " + pd.db + " without a query.\n"
                                        + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                      )
                                    )
                                    BasicLogService.tweet( "Reader departing spaceLock AgentKVDBNode Version 2 on " + this + " for mget on " + path + "." )
                                    //spaceLock.depart( Some( rk ) )
                                    spaceLock.depart( path, rk )
                                    //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                    //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

                                    rk(oV)
                                  }
                                  case Some(qry) => {
                                    BasicLogService.tweet( 
                                      (
                                        "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                        + "mgetting " + path + ".\n"
                                        + "on " + this + " from db " + pd.db + "\n"
                                        + "from coll " + xmlCollName + "\n"
                                        + "with query: " + qry
                                        + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                      )
                                    )                                    
                                    
                                    val rslts = executeWithResults(xmlCollName, qry)
                                    
                                    rslts match {
                                      case Nil => {

                                        BasicLogService.tweet(
                                          (
                                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                              + "mgetting " + path + ".\n"
                                              + "on " + this + " from db " + pd.db + "\n"
                                              + "from coll " + xmlCollName + "\n"
                                              + "with query: " + qry
                                              + "with keep : " + keep
                                              + " had no matching resources."
                                              + "\n-----------------------------------------------------------------"
                                              + "\nchecking to store continuation query"
                                              + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                            )
                                        )

                                        storeKQuery(xmlCollName, pd)(path, keep, rk)

                                        // Then forward the request
                                        //forward( ask, hops, path )

                                        BasicLogService.tweet( "Reader departing spaceLock AgentKVDBNode Version 3 on " + this + " for mget on " + path + "." )
                                        //spaceLock.depart( Some( rk ) )
                                        spaceLock.depart( path, rk )
                                        //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                        //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                                        
                                        rk(oV)
                                      }
                                      case _ => {
                                        BasicLogService.tweet( 
                                          (
                                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                            + "mgetting " + path + ".\n"
                                            + "on " + this + " from db " + pd.db + "\n"
                                            + "from coll " + xmlCollName + "\n"
                                            + "with query: ... " 
                                            + "\n had "
                                            + rslts.length
                                            + " matching resources."
                                            + "\n-----------------------------------------------------------------"
                                            + "\nstoring continuation query"
                                            + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                          )
                                        )
                                        
                                        // BUGBUG -- LGM : This is a
                                        // window of possible
                                        // failure; if we crash here,
                                        // then the result is out of
                                        // the store, but we haven't
                                        // completed processing. This is
                                        // where we need Tx.
                                        
                                        if ( cursor ) {
                                          var rsrcRslts: List[ mTT.Resource ] = Nil
                                          
                                          for ( rslt <- itergen[ Elem ](rslts) ) {
                                            BasicLogService.tweet("retrieved " + rslt.toString)
                                            
                                             consume match {
                                               case policy : RetainInStore => {
                                                 BasicLogService.tweet("removing from store " + rslt)
                                                 removeFromStore(
                                                   persist,
                                                   rslt,
                                                   collName
                                                 )
                                               }
                                               case _ => {
                                                 BasicLogService.tweet( "policy indicates not to remove from store " + rslt )
                                               }
                                            }
                                            
                                            val ersrc: emT.PlaceInstance = pd.asResource(path, rslt)
                                            ersrc.stuff match {
                                              case Left( r ) => rsrcRslts = r :: rsrcRslts
                                              case _ => {}
                                            }
                                          }

                                          val rsrcCursor = asCursor( rsrcRslts )

                                          // Need to store continuation if
                                          // this is a subscribe
                                          consume match {
                                            case policy : Subscription => {
                                              BasicLogService.tweet(
                                                "\n===================================================================\n"
                                                + "Storing subscription continuation"
                                                + "with keep : " + keep
                                                + "\n===================================================================\n"
                                              )
                                              storeKQuery( xmlCollName, pd )( path, keep, rk )
                                              rk( rsrcCursor )
                                            }
                                            case _ => {
                                              BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 10" + this + " for mget on " + path + "." )
                                              //spaceLock.depart( Some( rk ) )
                                              spaceLock.depart( path, Some( rk ) )
                                              rk( rsrcCursor )
                                            }
                                          }
                                          
                                        }
                                        else {
                                          consume match {
                                            case policy : Subscription => {
                                              BasicLogService.tweet(
                                                "\n===================================================================\n"
                                                + "Storing subscription continuation"
                                                + "with keep : " + keep
                                                + "\n===================================================================\n"
                                              )                                       
                                              for ( rslt <- itergen[ Elem ](rslts) ) {
                                                
                                                BasicLogService.tweet("retrieved " + rslt.toString)
                                            
                                                consume match {
                                                  case policy : RetainInStore => {
                                                    BasicLogService.tweet("removing from store " + rslt)
                                                    removeFromStore(
                                                      persist,
                                                      rslt,
                                                      collName
                                                    )
                                                  }
                                                  case _ => {
                                                    BasicLogService.tweet( "policy indicates not to remove from store" + rslt )
                                                  }
                                                }
                                              }

                                              storeKQuery( xmlCollName, pd )( path, keep, rk )

                                              for ( rslt <- itergen[ Elem ](rslts) ) {
                                                val ersrc = pd.asResource(path, rslt)
                                                BasicLogService.tweet("returning " + ersrc)                                             
                                                ersrc.stuff match {
                                                  case Left( r ) => rk( Some( r ) )
                                                  case _ => {}
                                                }
                                              }
                                            }
                                            case _ => {
                                              BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 10" + this + " for mget on " + path + "." )
                                              //spaceLock.depart( Some( rk ) )
                                              spaceLock.depart( path, Some( rk ) )
                                              for ( rslt <- itergen[ Elem ](rslts) ) {
                                            
                                                BasicLogService.tweet("retrieved " + rslt.toString)
                                                
                                                consume match {
                                                  case policy : RetainInStore => {
                                                    BasicLogService.tweet("removing from store " + rslt)
                                                    removeFromStore(
                                                      persist,
                                                      rslt,
                                                      collName
                                                    )
                                                  }
                                                  case _ => {
                                                    BasicLogService.tweet( "policy indicates not to remove from store" + rslt )
                                                  }
                                                }
                                                
                                                val ersrc = pd.asResource(path, rslt)
                                                BasicLogService.tweet("returning " + ersrc)                                             
                                                ersrc.stuff match {
                                                  case Left( r ) => rk( Some( r ) )
                                                  case _ => {}
                                                }
                                              }
                                            }
                                          }                                          
                                        }
                                        
                                      }
                                    }
                                  }
                                }
                              }
                              case false => {

                                BasicLogService.tweet( "Reader departing spaceLock AgentKVDBNode Version 5 on " + this + " for mget on " + path + "." )
                                spaceLock.depart( path, rk )
                                //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )

                                rk(oV)
                              }
                            }
                          }
                        }
                      }

                      case oCacheV@Some( cacheV ) => {                    
                        BasicLogService.tweet( 
                          (
                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                            + "mgetting " + path + ".\n"
                            + "on " + this + "found a result in memory cache.\n"
                            + cacheV
                            + "\ncleaning store"
                            + "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                          )
                        )
                        persist match {
                          case None => {
                            BasicLogService.tweet( 
                              (
                                "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                + "mgetting " + path + ".\n"
                                + "result in cache on " + this + "without a persistence manifest.\n"
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                              )
                            )
                            
                            
                            BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 7 " + this + " for mget on " + path + "." )
                            //spaceLock.depart( Some( rk ) )
                            spaceLock.depart( path, Some( rk ) )
                            //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                            //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                            if ( !cursor ) {
                              rk( oV )
                            }
                            else {
                              var rsrcRslts: List[ mTT.Resource ] = Nil
                              for ( rslt <- results ) {
                                BasicLogService.tweet("retrieved " + rslt.toString)
                          
                                rslt match {
                                  case Some(r) => rsrcRslts = r :: rsrcRslts
                                  case _ => {}
                                }
                              }
                              rk( asCursor(rsrcRslts) )
                            }
                          }
                          case Some( pd ) => {
                            val xmlCollName =
                              collName.getOrElse(
                                storeUnitStr.getOrElse(
                                  bail()
                                )
                              )
                            
                            // Defensively check that db is actually available
                            
                            checkIfDBExistsAndCreateIfNot( xmlCollName, true ) match {
                              case false => {
                                BasicLogService.tweet( 
                                  (
                                    "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                    + "mgetting " + path + ".\n"
                                    + "result in cache on " + this + " without a database.\n"
                                    + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                  )
                                )
                                
                                BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 8 " + this + " for mget on " + path + "." )
                                //spaceLock.depart( Some( rk ) )
                                spaceLock.depart( path, Some( rk ) )
                                //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                                
                                if ( !cursor ) {
                                  rk( oV )
                                }
                                else {
                                  var rsrcRslts: List[ mTT.Resource ] = Nil
                                  for ( rslt <- results ) {
                                    BasicLogService.tweet("retrieved " + rslt.toString)
                                    
                                    rslt match {
                                      case Some(r) => rsrcRslts = r :: rsrcRslts
                                      case _ => {}
                                    }
                                  }
                                  rk( asCursor(rsrcRslts) )
                                }
                              }
                              case true => {
                                query( xmlCollName, path ) match {
                                  case None => {
                                    BasicLogService.tweet( 
                                      (
                                        "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                        + "mgetting " + path + ".\n"
                                        + "result in cache on " + this + " without a query.\n"
                                        + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                      )
                                    )
                                    
                                    BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 9 " + this + " for mget on " + path + "." )
                                    //spaceLock.depart( Some( rk ) )
                                    spaceLock.depart( path, Some( rk ) )
                                    //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                    //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                                    
                                    if ( !cursor ) {
                                      rk( oV )
                                    }
                                    else {
                                      var rsrcRslts: List[ mTT.Resource ] = Nil
                                      for ( rslt <- results ) {
                                        BasicLogService.tweet("retrieved " + rslt.toString)
                                        
                                        rslt match {
                                          case Some(r) => rsrcRslts = r :: rsrcRslts
                                          case _ => {}
                                        }
                                      }
                                      rk( asCursor(rsrcRslts) )
                                    }
                                  }
                                  case Some( qry ) => {
                                    executeWithResults( xmlCollName, qry ) match {
                                      case Nil => {
                                        BasicLogService.tweet( 
                                          (
                                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                            + "mgetting " + path + ".\n"
                                            + "result in cache on " + this + " no matching results in store.\n"
                                            + "consume : " + consume + "\n"
                                            + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                          )
                                        )
                                        
                                        //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                        //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                                        
                                        // Need to store continuation if
                                        // this is a subscribe
                                        consume match {
                                          case policy : Subscription => {
                                            BasicLogService.tweet(
                                              "\n===================================================================\n"
                                              + "Storing subscription continuation"
                                              + "with keep : " + keep
                                              + "\n===================================================================\n"
                                            )
                                            storeKQuery( xmlCollName, pd )( path, keep, rk )
                                            if ( !cursor ) {
                                              rk( oV )
                                            }
                                            else {
                                              var rsrcRslts: List[ mTT.Resource ] = Nil
                                              for ( rslt <- results ) {
                                                BasicLogService.tweet("retrieved " + rslt.toString)
                                                
                                                rslt match {
                                                  case Some(r) => rsrcRslts = r :: rsrcRslts
                                                  case _ => {}
                                                }
                                              }
                                              rk( asCursor(rsrcRslts) )
                                            }
                                          }
                                          case _ => {
                                            BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 10" + this + " for mget on " + path + "." )
                                            //spaceLock.depart( Some( rk ) )
                                            spaceLock.depart( path, Some( rk ) )
                                            if ( !cursor ) {
                                              rk( oV )
                                            }
                                            else {
                                              var rsrcRslts: List[ mTT.Resource ] = Nil
                                              for ( rslt <- results ) {
                                                BasicLogService.tweet("retrieved " + rslt.toString)
                                                
                                                rslt match {
                                                  case Some(r) => rsrcRslts = r :: rsrcRslts
                                                  case _ => {}
                                                }
                                              }
                                              rk( asCursor(rsrcRslts) )
                                            }
                                          }
                                        }
                                        
                                      }
                                      case rslts => {
                                        BasicLogService.tweet( 
                                          (
                                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                            + "mgetting " + path + ".\n"
                                            + "result in cache on " + this + " had " + rslts.length + " matching results in store.\n"
                                            + "collection: " + xmlCollName + "\n"
                                            + "consume : " + consume + "\n"
                                            + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                                          )
                                        )
                                        
                                        // BUGBUG -- lgm : what to
                                        // do in the case that
                                        // what's in store doesn't
                                        // match what's in cache?
                                        for( rslt <- itergen[Elem]( rslts ) ) {
                                          BasicLogService.tweet( "retrieved " + rslt.toString )                                     
                                          
                                          consume match {
                                            case policy : RetainInStore => {
                                              BasicLogService.tweet( "removing from store " + rslt )
                                              removeFromStore( 
                                                persist,
                                                rslt,
                                                collName
                                              )
                                            }
                                            case _ => {
                                              BasicLogService.tweet( "policy indicates not to remove from store" + rslt )
                                            }
                                          }
                                          
                                        }
                                        
                                        //BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 11" + this + " for mget on " + path + "." )
                                        //spaceLock.depart( Some( rk ) )
                                        //spaceLock.depart( path, Some( rk ) )
                                        //BasicLogService.tweet( "spaceLock reading room: " + spaceLock.readingRoom )
                                        //BasicLogService.tweet( "spaceLock writing room: " + spaceLock.writingRoom )
                                        // Need to store continuation if
                                        // this is a subscribe
                                        consume match {
                                          case policy : Subscription => {
                                            BasicLogService.tweet(
                                              "\n===================================================================\n"
                                              + "Storing subscription continuation"
                                              + "with keep : " + keep
                                              + "\n===================================================================\n"
                                            )
                                            storeKQuery( xmlCollName, pd )( path, keep, rk )
                                            if ( !cursor ) {
                                              rk( oV )
                                            }
                                            else {
                                              var rsrcRslts: List[ mTT.Resource ] = Nil
                                              for ( rslt <- results ) {
                                                BasicLogService.tweet("retrieved " + rslt.toString)
                                                
                                                rslt match {
                                                  case Some(r) => rsrcRslts = r :: rsrcRslts
                                                  case _ => {}
                                                }
                                              }
                                              rk( asCursor(rsrcRslts) )
                                            }
                                          }
                                          case _ => {
                                            BasicLogService.tweet( "Reader departing spaceLock PMKVDBNode Version 10" + this + " for mget on " + path + "." )
                                            //spaceLock.depart( Some( rk ) )
                                            spaceLock.depart( path, Some( rk ) )
                                            if ( !cursor ) {
                                              rk( oV )
                                            }
                                            else {
                                              var rsrcRslts: List[ mTT.Resource ] = Nil
                                              for ( rslt <- results ) {
                                                BasicLogService.tweet("retrieved " + rslt.toString)
                                                
                                                rslt match {
                                                  case Some(r) => rsrcRslts = r :: rsrcRslts
                                                  case _ => {}
                                                }
                                              }
                                              rk( asCursor(rsrcRslts) )
                                            }
                                          }
                                        }                                         
                                      }
                                    }                                 
                                  }
                                }                                                                 
                              }                         
                            }
                          }
                        }                         
                      }
                    }
                  }
                }
            }
        }
      }
    }

    object BaseAgentKVDB {
      def apply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] ( 
        name : Moniker
      ) : BaseAgentKVDB[ReqBody,RspBody,KVDBNode] = {
        new BaseAgentKVDB[ReqBody,RspBody,KVDBNode]( name )
      }
      def unapply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] (
        pmkvdb : BaseAgentKVDB[ReqBody,RspBody,KVDBNode]
      ) : Option[( Moniker )] = {
        Some( ( pmkvdb.name ) )
      }
    }

    trait BaseAgentKVDBNodeFactoryT
      extends AMQPURIOps
      with ThreadPoolRunnersX
      //with FJTaskRunnersX
    {
      // BUGBUG -- lgm : So far, establishing appropriate bounds has
      // been too hard
      type AgentCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]
      //type AgentNode[Rq <: ReqBody,Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,AgentNode]

      def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        here : URI, configFileName : Option[String]
      ) : AgentCache[ReqBody,RspBody]
      // def ptToMany[Rq <: ReqBody, Rs <: RspBody]( here : URI, there : List[URI] ) : AgentNode[Rq,Rs]
//       def ptToPt[Rq <: ReqBody, Rs <: RspBody]( here : URI, there : URI ) : AgentNode[Rq,Rs]      
//       def loopBack[Rq <: ReqBody, Rs <: RspBody]( here : URI ) : AgentNode[Rq,Rs]
    }    

    implicit val defaultConfigFileNameOpt : Option[String] = None

    abstract class BaseAgentKVDBNode[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,+KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]](
      override val cache : BaseAgentKVDB[ReqBody,RspBody,KVDBNode], 
      override val acquaintances : List[Moniker],
      val cnxn : Option[acT.AgentCnxn],      
      val partitionMap : List[( acT.AgentCnxn, KVDBNode[ReqBody,RspBody] )]
    ) extends BasePersistedMonadicKVDBNode[ReqBody,RspBody,KVDBNode](
      cache, acquaintances
    ) with BaseAgentKVDBNodeFactoryT
    with PersistenceManifestTrampoline
    with XMLIfy[Namespace,Var] {
      import identityConversions._

      case class HashAgentKVDB[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        override val name : Moniker,
        implicit override val configFileName : Option[String]
      ) extends BaseAgentKVDB[ReqBody,RspBody,HashAgentKVDBNode](
        name
      ) 
      case class HashAgentKVDBNode[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        override val cache : HashAgentKVDB[ReqBody,RspBody],
        override val acquaintances : List[Moniker],
        override val cnxn : Option[acT.AgentCnxn],
        implicit override val configFileName : Option[String]
      ) extends BaseAgentKVDBNode[ReqBody,RspBody,HashAgentKVDBNode](
        cache, acquaintances, cnxn, Nil
      ) 

      //override type AgentCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] <: BaseAgentKVDB[ReqBody,RspBody,KVDBNode]
      //override type AgentNode[Rq <: ReqBody, Rs <: RspBody] = KVDBNode[Rq,Rs]

      override def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
        here : URI,
        configFileName : Option[String]
      ) : AgentCache[ReqBody,RspBody] = throw new Exception( "mkCache not implemented" )
      def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
        here : URI,
        configFileName : Option[String]
      ) : HashAgentKVDB[ReqBody,RspBody] = throw new Exception( "mkInnerache not implemented" )

      @transient
      var _cnxnPartition : Option[HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]]] = None
      def cnxnPartition : HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]] = {
        _cnxnPartition match { 
          case Some( pm ) => pm
          case None => {
            val pm = new HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]]( )
            for( ( cnxn, node ) <- partitionMap ) {
              node match {
                case hakvdbn : HashAgentKVDBNode[ReqBody,RspBody] => {
                  pm += ( cnxn -> hakvdbn )
                }
                case _ => {
                  BasicLogService.tweet( "warning: not hashing " + ( cnxn, node ) + " because node is not a compatible type" )
                }
              }
            }
            _cnxnPartition = Some( pm )
            pm
          }
          case null => {
            val pm = new HashMap[acT.AgentCnxn,HashAgentKVDBNode[ReqBody,RspBody]]( )
            for( ( cnxn, node ) <- partitionMap ) {
              node match {
                case hakvdbn : HashAgentKVDBNode[ReqBody,RspBody] => {
                  pm += ( cnxn -> hakvdbn )
                }
                case _ => {
                  BasicLogService.tweet( "warning: not hashing " + ( cnxn, node ) + " because node is not a compatible type" )
                }
              }
            }
            _cnxnPartition = Some( pm )
            pm
          }
        }
      }

      def makeSpace(
        cnxn : acT.AgentCnxn
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
        val symmIdStr = cnxn.symmetricIdentityString
        val nodeCache : HashAgentKVDB[ReqBody,RspBody] = 
          mkInnerCache(
            name.withPath(name.getPath + "/" + symmIdStr),
            configFileName
          )
        val oPM = nodeCache.persistenceManifest

        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : makeSpace "
            + "\n cnxn : " + cnxn
            + "\n this.cache : " + this.cache
            + "\n this.name : " + this.name
            + "\n --------------- "         
            + "\n symmIdStr : " + symmIdStr
            + "\n persistenceManifest : " + oPM
          )
        )                       

        val node : HashAgentKVDBNode[ReqBody,RspBody] =
          new HashAgentKVDBNode[ReqBody,RspBody](
            nodeCache,
            for( acq <- acquaintances ) yield { acq.withPath( acq.getPath + "/" + symmIdStr ) },
            Some( cnxn ),
            configFileName
          ) {
            override def persistenceManifest : Option[PersistenceManifest] = oPM
          }

        spawn { node.dispatchDMsgs() }
        
        node
      }

      def ptnCnxnWrapperNamespace : String = "patternConnection"

      def embedCnxn(
        cnxn : acT.AgentCnxn,
        ptn : CnxnCtxtLabel[Namespace,Var,Tag] with Factual
      ) : Option[CnxnCtxtLabel[Namespace,Var,Tag] with Factual] = {
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : embedCnxn "
            + "\n cnxn : " + cnxn
            + "\n ptn : " + ptn
            + "\n --------------- "
            + "\n labelToNS : " + labelToNS
            + "\n textToTag : " + textToTag
            + "\n persistenceManifest : " + persistenceManifest
          )
        )
        for(
          ltns <- labelToNS;
          ttt <- textToTag;
          pm <- persistenceManifest;
          if ( pm.isInstanceOf[XMLDBManifest] )
        ) yield {
          val xmldbPm = pm.asInstanceOf[XMLDBManifest]
          val xmlifier = xmldbPm.xmlIfier
          
          val embeddedCnxn =
            new CnxnCtxtBranch[Namespace,Var,Tag](
              ltns( ptnCnxnWrapperNamespace ),
              List(
                xmlIfier.tolabeledBlob [Namespace,Var,Tag]( ltns, ttt )( cnxn ),
                ptn
              )
            )
          //    BasicLogService.tweet(
          //      (
          //        ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
          //        + " embedding cnxn " + embeddedCnxn.toString
          //        + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
          //      )
          //    )
          embeddedCnxn
        }
      }
      
      def extractCnxn(
        ccl : CnxnCtxtLabel[Namespace,Var,Tag]
      ) : Option[( acT.AgentCnxn, CnxnCtxtLabel[Namespace,Var,Tag] with Factual )] = {
        ccl match {
          case CnxnCtxtBranch( ptnCnxnNS, cnxnBlob :: rs :: Nil ) => {
            for(
              pm <- persistenceManifest;
              if ( pm.isInstanceOf[XMLDBManifest] )           
            ) yield {
              //            BasicLogService.tweet(
              //              (
              //                ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
              //                + " embedded cnxn " + ccl.toString
              //                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
              //              )
              //            )
              val xmldbPm = pm.asInstanceOf[XMLDBManifest]
              val xmlifier = xmldbPm.xmlIfier
              val prod =
                xmlifier.fromlabeledBlob( cnxnBlob ).getOrElse(
                  throw new Exception( "must have a cnxn" )
                )
              ( prod.asInstanceOf[acT.AgentCnxn], rs )
            }
          }
          case _ => {
            None
          }
        }
      }

      /* --------------------------------------------------------------------
       *                     Aggregation management API
       * -------------------------------------------------------------------- */
      
      def cnxnMatch(
        cnxn1 : acT.AgentCnxn,
        cnxn2 : acT.AgentCnxn
      ) : Boolean = {
        
        BasicLogService.tweet(
          (
            "matching " + cnxn1 + " with " + cnxn2 + "\n"
            + "cnxn1.src = " + cnxn1.src
            + " cnxn2.src = " + cnxn2.src + "\n"
            + " src's "
            + (
              if ( cnxn1.src == cnxn2.src ) {
                " match "
              }
              else {
                " do not match "
              }
            )
            + "\n"
            + "cnxn1.trgt = " + cnxn1.trgt
            + " cnxn2.trgt = " + cnxn2.trgt + "\n"
            + " trgt's "
            + (
              if ( cnxn1.trgt == cnxn2.trgt ) {
                " match "
              }
              else {
                " do not match "
              }
            )
            + "\n"
            + "cnxn1.label = " + cnxn1.label
            + " cnxn2.label = " + cnxn2.label + "\n"
            + " label's "
            + (
              if ( cnxn1.label == cnxn2.label ) {
                " match "
              }
              else {
                " do not match "
              }
            )
          )
          
        )
        
        val rslt =
          (( cnxn1 == cnxn2 )
           || (( cnxn1.src == cnxn2.src )
               && ( cnxn1.trgt == cnxn2.trgt )
               && ( cnxn1.label == cnxn2.label)));
        
        BasicLogService.tweet( if ( rslt ) "matched" else "did not match" )
        
        rslt
      }    
      
      def searchCnxnPartition(
        cnxn : acT.AgentCnxn
      ) : Option[HashAgentKVDBNode[ReqBody,RspBody]] = {
        // local recursion that stops just when the match is found
        def search(
          pairs : List[( acT.AgentCnxn, HashAgentKVDBNode[ReqBody,RspBody] )]
        ) : Option[( acT.AgentCnxn, HashAgentKVDBNode[ReqBody,RspBody] )] = {
          pairs match {
            case ( cnxnKey, part ) :: rpairs => {
              if ( cnxnMatch( cnxn, cnxnKey ) ) {
                Some( ( cnxnKey, part ) )
              }
              else {
                search( rpairs )
              }
            }
            case Nil => None
          }
        }
        
        BasicLogService.tweet(
          "Map failed searching map pairs : " + cnxnPartition.toList
        )
        
        for( ( cnxnKey, part ) <- search( cnxnPartition.toList ) ) 
        yield { part }       
      }
      
      def getPartition(
        cnxn : acT.AgentCnxn
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
        cnxnPartition.get( cnxn ) match {
          // BUGBUG -- LGM : this is a workaround until we have
          // invitation and introduction protocols for PlatformAgents
          case None => {
            searchCnxnPartition( cnxn ) match {
              case None => {
                BasicLogService.tweet(
                  (
                    "No matching space for "
                    + cnxn
                    + "\n"
                    + "Creating a new one"
                  )
                )
                val npmgj = makeSpace( cnxn )
                //spawn { npmgj.dispatchDMsgs() }
                cnxnPartition( cnxn ) = npmgj
                npmgj
              }
              case Some( npmgj ) => {
                BasicLogService.tweet(
                  "Found cnxn matching through search " + cnxn + "\n"
                )
                npmgj
              }
            }
          }
          case Some( npmgj ) => {
            BasicLogService.tweet(
              "Found matching space for " + cnxn
            )
            npmgj
          }
        }
      }
      
      def getLocalPartition(
        cnxn : acT.AgentCnxn   // C( localProvider, l, remoteRequester )
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
        BasicLogService.tweet(
          "Getting local partition using " + cnxn
        )
        getPartition( cnxn )   // C( localProvider, l, remoteRequester )
      }
      
      def getRemotePartition(
        cnxn : acT.AgentCnxn   // C( remoteRequester, l, localProvider )
      ) : HashAgentKVDBNode[ReqBody,RspBody] = {
        BasicLogService.tweet(
          "Getting remote partition using " + cnxn
        )
        //       val rvrsCnxn =         // C( localProvider, l, remoteRequester )
        //      acT.AgentCnxn( cnxn.trgt, cnxn.label, cnxn.src )
        
        //       getLocalPartition( rvrsCnxn )
        // Since there is now a queue/partition we could only have
        // received this on the partition handling this queue; hence,
        // there is no further need for lookup
        cnxnPartition.get( cnxn ) match {
          case Some( rp ) => rp 
          case None => {
            val rp = new HashAgentKVDBNode[ReqBody,RspBody](
              cache.asInstanceOf[HashAgentKVDB[ReqBody,RspBody]],
              acquaintances,
              Some( cnxn ),
              configFileName
            )
            cnxnPartition += ( cnxn -> rp )
            rp
          }
        }
      }
      
      def getPartitionActuals(
        cnxn : acT.AgentCnxn,
        partFn : acT.AgentCnxn => HashAgentKVDBNode[ReqBody,RspBody]
      ) = {
        val pmgj : HashAgentKVDBNode[ReqBody,RspBody] = partFn( cnxn )
        val perD = pmgj.cache.persistenceManifest
        val xmlCollName = 
          perD match {
            case None => None
            case Some( pd ) => Some( pd.storeUnitStr( cnxn ) )
          }
        ( pmgj, perD, xmlCollName )
      }
      
      def getLocalPartitionActuals( cnxn : acT.AgentCnxn ) = {
        getPartitionActuals( cnxn, getLocalPartition )
      }
      
      def getRemotePartitionActuals( cnxn : acT.AgentCnxn ) = {
        getPartitionActuals( cnxn, getRemotePartition )
      }
      
      def forward( cnxn : acT.AgentCnxn )(
        ask : dAT.AskNum,
        hops : List[Moniker],
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      ) : Unit = {
        
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : forward "
            + "\n ask : " + ask
            + "\n hops : " + hops
            + "\n path : " + path
            + "\n --------------- "
            + "\n acquaintances : " + acquaintances
          )
        )               

        def loop( locations : List[URI], there : URI ) : Boolean = {
          ( false /: locations )(
            {
              ( acc, e ) => {
                val same = there.equals( e )
                // BasicLogService.tweet(
//                ( "comparing " + e + " with " + there + " : " + same )
//              )
                acc || same
              }
            }
          )
        }

        for(
          trgt <- acquaintances;
          q <- stblQMap.get( trgt )
          if ( ! ( loop( hops.map( _.uri ), trgt.uri ) ) )
        ) {
          val embCnxn = 
            embedCnxn(
              cnxn,
              path.asInstanceOf[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
            )
         BasicLogService.tweet(
           (
             "BaseAgentKVDBNode : "
             + "\nthis: " + this
             + "\n method : forward "
             + "\n ask : " + ask
             + "\n hops : " + hops
             + "\n path : " + path
             + "\n --------------- "
             + "\n trgt : " + trgt
             + "\n q : " + q
             + "\n embeddedCnxn: " + embCnxn
           )
        )
          // BUGBUG -- LGM: fix typing so we don't have to cast
          for(
            embeddedCnxn
            <- embedCnxn(
              cnxn,
              path.asInstanceOf[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
            )
          ) {
            val request = 
              ask match {
                case dAT.AGetNum => {
                  Msgs.MDGetRequest[Namespace,Var,Tag,Value](
                    embeddedCnxn
                  ).asInstanceOf[Msgs.DReq]
                }
                case dAT.AFetchNum => {
                  Msgs.MDFetchRequest[Namespace,Var,Tag,Value](
                    embeddedCnxn
                  ).asInstanceOf[Msgs.DReq]
                }
                case dAT.ASubscribeNum => {
                  Msgs.MDSubscribeRequest[Namespace,Var,Tag,Value](
                    embeddedCnxn
                  ).asInstanceOf[Msgs.DReq]
                }
                case _ => throw new Exception( "askType not handled" )
              }
            
            request match {
              case rqbdy : ReqBody => {
                val framedReq = frameRequest( trgt )( rqbdy )
                BasicLogService.tweet( ( "BaseAgentKVDBNode: " + this + " forwarding " + framedReq + " to " + trgt ) )
                q ! framedReq
              }
              case _ => {
                throw new Exception( "unable to frame request: " + request )
              }
            }
          }
        }
      }

      /* --------------------------------------------------------------------
       *                     The underlying API
       * -------------------------------------------------------------------- */

      def mget( cnxn : acT.AgentCnxn )(
        persist : Option[PersistenceManifest],
        ask : dAT.AskNum,
        hops : List[Moniker]
      )(
        channels : Map[mTT.GetRequest,mTT.Resource],
        registered : Map[mTT.GetRequest,List[RK]],
        consume : RetentionPolicy,
        keep : RetentionPolicy,
        cursor : Boolean,
        collName : Option[String]
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {        
        BasicLogService.tweet(
          (
            "PersistedMonadicKVDBNode : "
            + "\nmethod : mget "
            + "\nthis : " + this
            + "\ncnxn : " + cnxn
            + "\nchannels : " + channels
            + "\nregistered : " + registered
            + "\nconsume : " + consume
            + "\nkeep : " + keep
            + "\ncursor : " + cursor
            + "\ncollName : " + collName
            + "\npath : " + path
            + "\n---------------------------------------"
            + "\nchecking if this is a cache or a node: "
            + "node" + "\n"
          )
        )
        Generator {
          rk : ( Option[mTT.Resource] => Unit @suspendable ) =>
            shift {
              outerk : ( Unit => Unit ) =>
                reset {
                  BasicLogService.tweet( 
                    (
                      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                      + "mgetting " + path + ".\n"
                      + "on " + this + ".\n"
                      + "checking local cache " + cache + ".\n"
                      + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                    )
                  )
                  for(
                    oV <- cache.mget( cnxn )( persist, ask, hops )( channels, registered, consume, keep, cursor, collName )( path ) 
                  ) {
                    oV match {
                      case None => {
                        //BasicLogService.tweet( ">>>>> forwarding..." )
                        BasicLogService.tweet( 
                          (
                            "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                            + "mgetting " + path + ".\n"
                            + "on " + cache + " did not find a resource.\n"
                            + "xml collection name: " + collName + "\n"
                            + "consume data : " + true + "\n"
                            + "keep continuation : " + true + "\n"
                            + "forwarding to acquaintances.\n"
                            + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n"
                          )
                        )
                        
                        forward( cnxn )( ask, hops, path )
                        rk( oV )
                      }
                      case _ => rk( oV )
                    }
                  }
                }
            }
        }      
      }

      /* --------------------------------------------------------------------
       *                     The standard API
       * -------------------------------------------------------------------- */
      
      def put( cnxn : acT.AgentCnxn )(
        ptn : mTT.GetRequest, rsrc : mTT.Resource
      ) = {
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : put "
            + "\n cnxn : " + cnxn
            + "\n ptn : " + ptn
            + "\n rsrc : " + rsrc
            + "\n this.cache : " + this.cache
            + "\n this.name : " + this.name
          )
        )
        
        BasicLogService.tweet(
          "Partitions : " + cnxnPartition.toList
          
        )
        
        val ( pmgj, perD, xmlCollName ) =
          getLocalPartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Partitions : " + cnxnPartition.toList
          
        )
        
        BasicLogService.tweet(
          "Storing " + ptn + " " + rsrc + " in partition " + pmgj
          
        )
        
        pmgj.cache.mput( perD )(
          pmgj.theMeetingPlace, pmgj.theWaiters, true, xmlCollName
        )( ptn, rsrc )
      }
      
      def publish( cnxn : acT.AgentCnxn )(
        ptn : mTT.GetRequest, rsrc : mTT.Resource
      ) = {
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : publish "
            + "\n cnxn : " + cnxn
            + "\n ptn : " + ptn
            + "\n rsrc : " + rsrc
            + "\n this.cache : " + this.cache
            + "\n this.name : " + this.name
          )
        )
        val ( pmgj, perD, xmlCollName ) = getLocalPartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Publishing " + rsrc + " on " + ptn + " in partition " + pmgj
          
        )
        
        pmgj.cache.mput( perD )(
          pmgj.theChannels, pmgj.theSubscriptions, false, xmlCollName
        )( ptn, rsrc )
      }

      def remotePut( cnxn : acT.AgentCnxn )(
        ptn : mTT.GetRequest, rsrc : mTT.Resource
      ) = {
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : remotePut "
            + "\n cnxn : " + cnxn
            + "\n ptn : " + ptn
            + "\n rsrc : " + rsrc
            + "\n this.cache : " + this.cache
            + "\n this.name : " + this.name
          )
        )
        
        val ( pmgj, perD, xmlCollName ) =       getRemotePartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Storing " + ptn + " " + rsrc + " in partition " + pmgj
          
        )
        
        pmgj.cache.mput( perD )(
          pmgj.cache.theMeetingPlace, pmgj.cache.theWaiters, true, xmlCollName
        )( ptn, rsrc )
      }
      
      def remotePublish( cnxn : acT.AgentCnxn )(
        ptn : mTT.GetRequest, rsrc : mTT.Resource
      ) = {
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : remotePublish "
            + "\n cnxn : " + cnxn
            + "\n ptn : " + ptn
            + "\n rsrc : " + rsrc
            + "\n this.cache : " + this.cache
            + "\n this.name : " + this.name
          )
        )
        val ( pmgj, perD, xmlCollName ) = getRemotePartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Publishing " + rsrc + " on " + ptn + " in partition " + pmgj
          
        )
        
        pmgj.cache.mput( perD )(
          pmgj.cache.theChannels, pmgj.cache.theSubscriptions, false, xmlCollName
        )( ptn, rsrc )
      }

      def get( hops : List[Moniker] )(
        cursor : Boolean
      )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        BasicLogService.tweet(
          "In cnxn-based get with cnxn " + cnxn
          
        )
        
        BasicLogService.tweet(
          "Partitions : " + cnxnPartition.toList
          
        )
        
        val ( pmgj, perD, xmlCollName ) =
          getLocalPartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Partitions : " + cnxnPartition.toList
          
        )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj
          
        )
        
        pmgj.mget( cnxn )( perD, dAT.AGetNum, hops )(
          pmgj.theMeetingPlace, pmgj.theWaiters, CacheAndStore, Store, cursor, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }    
      
      def get(
        cursor : Boolean
      )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        get( Nil )( cursor )( cnxn )( path )
      }
      
      def get(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        get( Nil )( false )( cnxn )( path )
      }

      def remoteGet( hops : List[Moniker] )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        BasicLogService.tweet(
          (
            "BaseAgentKVDBNode : "
            + "\nthis: " + this
            + "\n method : remoteGet "
            + "\n cnxn : " + cnxn
            + "\n path : " + path
            + "\n hops : " + hops
            + "\n this.cache : " + this.cache
            + "\n this.name : " + this.name
          )
        )
        
        val ( pmgj, perD, xmlCollName ) = getRemotePartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj          
        )
        
        pmgj.mget( cnxn )( perD, dAT.AGetNum, hops )(
          pmgj.theMeetingPlace, pmgj.theWaiters, CacheAndStore, Store, false, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }

      def fetch( hops : List[Moniker] )(
        cursor: Boolean
      )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {   
        BasicLogService.tweet(
          "In cnxn-based fetch with cnxn " + cnxn
          
        )
        
        val ( pmgj, perD, xmlCollName ) = getLocalPartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj
          
        )
        
        pmgj.mget( cnxn )( perD, dAT.AFetchNum, hops )(
          pmgj.theMeetingPlace, pmgj.theWaiters, DoNotRetain, Store, cursor, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }
      
      def fetch(
        cursor: Boolean
      )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {        
        fetch( Nil )( cursor )( cnxn )( path )
      }
      
      def fetch(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        fetch( Nil )( false )( cnxn )( path )
      }            

      def remoteFetch( hops : List[Moniker] )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {   
        BasicLogService.tweet(
          "In cnxn-based fetch with cnxn " + cnxn
          
        )
        
        val ( pmgj, perD, xmlCollName ) =       getRemotePartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj
          
        )
        
        pmgj.mget( cnxn )( perD, dAT.AFetchNum, hops )(
          pmgj.theMeetingPlace, pmgj.theWaiters, DoNotRetain, Store, false, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }
      
      
      def read( hops : List[Moniker] )(
        cursor: Boolean
      )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {   
        BasicLogService.tweet(
          "In cnxn-based read with cnxn " + cnxn
          
        )
        
        val ( pmgj, perD, xmlCollName ) = getLocalPartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj
          
        )
        
        pmgj.mget( cnxn )( perD, dAT.AFetchNum, hops )(
          pmgj.theMeetingPlace, pmgj.theWaiters, DoNotRetain, DoNotRetain, cursor, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }
      
      def read(
        cursor: Boolean
      )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {        
        read( Nil )( cursor )( cnxn )( path )
      }
      
      def read(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {
        read( Nil )( false )( cnxn )( path )
      }            
      

      def subscribe( hops : List[Moniker] )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {    
        BasicLogService.tweet(
          "In cnxn-based subscribe with cnxn " + cnxn
          
        )
        
        val ( pmgj, perD, xmlCollName ) = getLocalPartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj
          
        )
        
        pmgj.mget( cnxn )( perD, dAT.ASubscribeNum, hops )(
          pmgj.theChannels, pmgj.theSubscriptions, CacheAndStoreSubscription, Store, false, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }
      
      def subscribe(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {        
        subscribe( Nil )( cnxn )( path )    
      }
      
      def remoteSubscribe( hops : List[Moniker] )(
        cnxn : acT.AgentCnxn
      )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )
      : Generator[Option[mTT.Resource],Unit,Unit] = {    
        BasicLogService.tweet(
          "In cnxn-based subscribe with cnxn " + cnxn
          
        )
        
        val ( pmgj, perD, xmlCollName ) = getRemotePartitionActuals( cnxn )
        
        BasicLogService.tweet(
          "Retrieving " + path + " from partition " + pmgj
          
        )
        
        pmgj.mget( cnxn )( perD, dAT.ASubscribeNum, hops )(
          pmgj.theChannels, pmgj.theSubscriptions, CacheAndStoreSubscription, Store, false, xmlCollName
        )( path ).asInstanceOf[Generator[Option[mTT.Resource],Unit,Unit]]
      }            

      def store(
        cnxn : acT.AgentCnxn
      )(
        ptn : mTT.GetRequest,
        rsrc : mTT.Resource
      ) : Unit = {
        // BUGBUG -- LGM this should store at all patterns
        // It looks as if putPlaces is not doing the full calculation
        BasicLogService.tweet(
          "In cnxn-based store with cnxn " + cnxn
          
        )
        
        val pmgj = getLocalPartition( cnxn )
        
        BasicLogService.tweet(
          "In cnxn-based store with partition " + pmgj
          
        )
        
        for( pd <- pmgj.cache.persistenceManifest ) {
          spawn {
            val rcrd = pd.asStoreRecord( ptn, rsrc )
            BasicLogService.tweet(
              (
                "storing to db : " + pd.db
                + " pair : " + rcrd
                + " in coll : " + pd.storeUnitStr( cnxn )
              )
            )
            pmgj.cache.store( pd.storeUnitStr( cnxn ) )( rcrd )
          }
        }
      }    
      
      def delete(
        cnxn : acT.AgentCnxn
      )(
        path : mTT.GetRequest
      )
      : Unit = {
        BasicLogService.tweet(
          "In cnxn-based delete with cnxn " + cnxn
        )
        
        val pmgj = getLocalPartition( cnxn )
        
        BasicLogService.tweet(
          "In cnxn-based delete with partition " + pmgj
        )
        
        for( pd <- pmgj.cache.persistenceManifest ) {
          spawn {
            reset{
              for( pI <- pmgj.delete( path ) ) {
                BasicLogService.tweet( "place deleted from cache: " + pI )
              }
            }
            BasicLogService.tweet(
              "deleting from db : " + pd.db
              + " key : " + path.toString
              + " in coll : " + pd.storeUnitStr( cnxn ) 
            )

            path match {
              case cclStr : CnxnCtxtLabel[Namespace,Var,String] => {
                pmgj.cache.delete(pd.storeUnitStr( cnxn ), cclStr)
              }
              case _ => {
                BasicLogService.tweet( "warning: unable to delete key from db" )
              }
            }       
          }
        }
      }
      
      def drop ( cnxn:acT.AgentCnxn )
      : Unit = {
        BasicLogService.tweet(
          "In cnxn-based drop with cnxn " + cnxn
        )
        
        val pmgj = getLocalPartition( cnxn )
        
        BasicLogService.tweet(
          "In cnxn-based drop with partition " + pmgj
        )
        
        for( pd <- pmgj.cache.persistenceManifest ) {
          spawn {
            BasicLogService.tweet(
              "dropping coll " + pd.storeUnitStr( cnxn ) 
            )
            pmgj.cache.drop(pd.storeUnitStr( cnxn ))
          }
        }
      }           

      def pullCnxnKRecords( cnxn : acT.AgentCnxn )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      ) : List[emT.PlaceInstance]
      = {
        val ( pmgj, perD, xmlCollName ) =
          getLocalPartitionActuals( cnxn )

        pmgj.cache.pullKRecords(
          perD,
          path,
          xmlCollName
        ).getOrElse( List[emT.PlaceInstance]( ) )
      }

      def resubmitRequests( cnxn : acT.AgentCnxn )(
        placeInstances : List[emT.PlaceInstance]
      )(
        implicit resubmissionAsk : dAT.AskNum
      ) : Option[HashAgentKVDBNode[ReqBody,RspBody]#Generator[emT.PlaceInstance,Unit,Unit]] = {

        val ( pmgj, perD, xmlCollName ) = getLocalPartitionActuals( cnxn )

        pmgj.resubmitRequests( perD, placeInstances, xmlCollName )( resubmissionAsk )
      }

      def resubmitGet( cnxn : acT.AgentCnxn )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
            ) : Option[HashAgentKVDBNode[ReqBody,RspBody]#Generator[emT.PlaceInstance,Unit,Unit]]
            = {
        resubmitLabelRequests(cnxn)(path)(dAT.AGetNum)
      }

      def resubmitLabelRequests( cnxn : acT.AgentCnxn )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )(
        implicit resubmissionAsk : dAT.AskNum
      ) : Option[HashAgentKVDBNode[ReqBody,RspBody]#Generator[emT.PlaceInstance,Unit,Unit]]
      = {
        val ( pmgj, perD, xmlCollName ) =
          getLocalPartitionActuals( cnxn )
        val placeInstances : List[emT.PlaceInstance] =
          pmgj.cache.pullKRecords(
            perD,
            path,
            xmlCollName
          ).getOrElse( List[emT.PlaceInstance]( ) )

        resubmitRequests( cnxn )( placeInstances )( resubmissionAsk )
      }      

      def resubmitCnxnLabelRequests( cnxns : List[acT.AgentCnxn] )(
        path : CnxnCtxtLabel[Namespace,Var,Tag]
      )(
        implicit resubmissionAsk : dAT.AskNum
      ) : List[Option[HashAgentKVDBNode[ReqBody,RspBody]#Generator[emT.PlaceInstance,Unit,Unit]]]
      = {
        for( cnxn <- cnxns ) yield {
          val ( pmgj, perD, xmlCollName ) =
            getLocalPartitionActuals( cnxn )
          val placeInstances : List[emT.PlaceInstance] =
            pmgj.cache.pullKRecords(
              perD,
              path,
              xmlCollName
            ).getOrElse( List[emT.PlaceInstance]( ) )
          
          resubmitRequests( cnxn )( placeInstances )( resubmissionAsk )
        }
      }

      /* --------------------------------------------------------------------
       *                     The dispatching section
       * -------------------------------------------------------------------- */

      override def dispatchDMsg( dreq : FramedMsg ) : Unit = {
        BasicLogService.tweet(
            (
              "BaseAgentKVDBNode : "
              + "\nmethod : dispatchDMsg "
              + "\nthis : " + this
              + "\ndreq : " + dreq
            )
          )
        dreq match {
          case Left( JustifiedRequest( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
            BasicLogService.tweet(
              (
                "BaseAgentKVDBNode : "
                + "\nmethod : dispatchDMsg "
                + "\nthis : " + this
                + "\n handling a JustifiedRequest "
                + "\n msgId : " + msgId
                + "\n mtrgt : " + mtrgt
                + "\n msrc : " + msrc
                + "\n lbl : " + lbl
                + "\n body : " + body
              )
            )
            body match {
              case dgreq@Msgs.MDGetRequest( path ) => {   
                BasicLogService.tweet(
                  (
                    "BaseAgentKVDBNode : "
                    + "\nmethod : dispatchDMsg "
                    + "\nthis : " + this
                    + "\n handling an MDGetRequest "
                    + "\n path : " + path
                  )
                )
                val cnxnNPath = extractCnxn( path )
                BasicLogService.tweet(
                  (
                    "BaseAgentKVDBNode : "
                    + "\nmethod : dispatchDMsg "
                    + "\nthis : " + this
                    + "\n cnxnNPath : " + cnxnNPath
                  )
                )
                for( ( cnxn, npath ) <- cnxnNPath ) {
                  BasicLogService.tweet( ( this + " getting locally for location : " + path ) )
                  reset {
                    for( v <- remoteGet( List( msrc ) )( cnxn )( npath ) ) {
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
              }
              
              case dfreq@Msgs.MDFetchRequest( path ) => {
                for( ( cnxn, npath ) <- extractCnxn( path ) ) {
                  BasicLogService.tweet( ( this + "fetching locally for location : " + npath ) )
                  reset {
                    for( v <- remoteFetch( List( msrc ) )( cnxn )( npath ) ) {
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
              }
              
              case dsreq@Msgs.MDSubscribeRequest( path ) => {
                for( ( cnxn, npath ) <- extractCnxn( path ) ) {
                  BasicLogService.tweet( ( this + "subscribing locally for location : " + npath ) )
                  reset {
                    for( v <- remoteSubscribe( List( msrc ) )( cnxn )( npath ) ) {
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
              }
              
              case dpreq@Msgs.MDPutRequest( path, value ) => {  
                reset { cache.put( path, mTT.Ground( value ) ) }
                for( q <- stblQMap.get( msrc ) ) {
                  q ! wrapResponse( msrc, dpreq )
                }
              }
              case dpbreq@Msgs.MDPublishRequest( path, value ) => {     
                reset { cache.publish( path, mTT.Ground( value ) ) }
                for( q <- stblQMap.get( msrc ) ) {
                  q ! wrapResponse( msrc, dpbreq )
                }
              }
            }
          }
          // BUGBUG -- lgm : DRY this please
          case Right( JustifiedResponse( msgId, mtrgt, msrc, lbl, body, _ ) ) => {
            body match {
              case RsrcMsgs.MDGetResponseRsrc( path, rsrc ) => {
                for( ( cnxn, npath ) <- extractCnxn( path ) ) {
                  rsrc match {
                    // if the rsrc comes with a substitution
                    // apply that to the path
                    case rbnd : mTT.RBound => {
                      rsrc( npath ) match {
                        // if the application results in a specialization
                        // put the results there
                        case Some( spec ) => {
                          for( inrRsrc <- rbnd.rsrc ) {                   
                            reset { remotePut( cnxn )( spec, inrRsrc ) }
                          }                               
                        }
                        // else put the results at the path
                        case None => {
                          reset { remotePut( cnxn )( npath, rsrc ) }
                        }
                      }           
                    }
                    case _ => {
                      reset { remotePut( cnxn )( npath, rsrc ) }
                    }
                  }
                }
              }
              case RsrcMsgs.MDFetchResponseRsrc( path, rsrc ) => {
                for( ( cnxn, npath ) <- extractCnxn( path ) ) {
                  rsrc match {
                    case rbnd : mTT.RBound => {
                      rsrc( npath ) match {
                        case Some( spec ) => {
                          for( inrRsrc <- rbnd.rsrc ) {
                            reset { remotePut( cnxn )( spec, inrRsrc ) }
                          }                               
                        }
                        case None => {
                          reset { remotePut( cnxn )( npath, rsrc ) }
                        }
                      }           
                    }
                    case _ => {
                      reset { remotePut( cnxn )( npath, rsrc ) }
                    }
                  }
                }
              }
              case RsrcMsgs.MDSubscribeResponseRsrc( path, rsrc ) => {
                for( ( cnxn, npath ) <- extractCnxn( path ) ) {
                  rsrc match {
                    case rbnd : mTT.RBound => {
                      rsrc( npath ) match {
                        case Some( spec ) => {
                          for( inrRsrc <- rbnd.rsrc ) {
                            reset { remotePublish( cnxn )( spec, inrRsrc ) }
                          }                               
                        }
                        case None => {
                          reset { remotePublish( cnxn )( npath, rsrc ) }
                        }
                      }           
                    }
                    case _ => {
                      reset { remotePublish( cnxn )( npath, rsrc ) }
                    }
                  }
                }
              }     
              case dput : RsrcMsgs.MDPutResponse[Namespace,Var,Tag,Value] => {  
              }
              case _ => {
                BasicLogService.tweet(
                  (
                    this 
                    + " handling unexpected message : "
                    + body
                  )
                )
              }
            }
          }
        }
      }
    }
    
    object BaseAgentKVDBNode {
      // def apply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] ( 
//      cache : BaseAgentKVDB[ReqBody,RspBody,KVDBNode],
//      acquaintances : List[Moniker],
//      cnxn : Option[acT.AgentCnxn],
//      cnxnPartition : List[( acT.AgentCnxn, KVDBNode[ReqBody,RspBody] )]
//       ) : BaseAgentKVDBNode[ReqBody,RspBody,KVDBNode] = {
//      new BaseAgentKVDBNode[ReqBody,RspBody,KVDBNode]( cache, acquaintances, cnxn, cnxnPartition )
//       }
      def unapply [ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse,KVDBNode[Rq <: ReqBody, Rs <: RspBody] <: BaseAgentKVDBNode[Rq,Rs,KVDBNode]] (
        pmkvdbnode : BaseAgentKVDBNode[ReqBody,RspBody,KVDBNode]
      ) : Option[( BaseAgentKVDB[ReqBody,RspBody,KVDBNode], List[Moniker], Option[acT.AgentCnxn], List[( acT.AgentCnxn,KVDBNode[ReqBody,RspBody] )] )] = {
        Some( ( pmkvdbnode.cache, pmkvdbnode.acquaintances, pmkvdbnode.cnxn, pmkvdbnode.partitionMap ) )
      }
    }

    case class AgentKVDB[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      override val name : Moniker,
      implicit override val configFileName : Option[String]
    ) extends BaseAgentKVDB[ReqBody,RspBody,AgentKVDBNode](
      name
    )     

    case class AgentKVDBNode[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      override val cache : AgentKVDB[ReqBody,RspBody],
      override val acquaintances : List[Moniker],
      override val cnxn : Option[acT.AgentCnxn],
      implicit override val configFileName : Option[String]
    ) extends BaseAgentKVDBNode[ReqBody,RspBody,AgentKVDBNode](
      cache, acquaintances, cnxn, Nil
    ) 

    trait AgentKVDBNodeFactoryT
      extends AMQPURIOps
      with ThreadPoolRunnersX
      //with FJTaskRunnersX
    {
      def ptToMany[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        here : URI, there : List[URI]
      )( implicit configFileNameOpt : Option[String] ) : AgentKVDBNode[ReqBody,RspBody]
      def ptToPt[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        here : URI, there : URI
      )( implicit configFileNameOpt : Option[String] ) : AgentKVDBNode[ReqBody,RspBody]      
      def loopBack[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
        here : URI
      )( implicit configFileNameOpt : Option[String] ) : AgentKVDBNode[ReqBody,RspBody]
    }
  }
}

case class PutVal(
  values : List[Int],
  dstypes : List[String],
  dsnames : List[String],
  time : Double,
  interval : Double,
  host : String,
  plugin : String,
  plugin_instance : String,
  `type` : String,
  type_instance : String
)

package usage {
  //import com.biosimilarity.lift.lib.bulk._

  //import net.liftweb.json._

  import scala.xml._
  import scala.xml.XML._
  import scala.collection.mutable.Buffer
  import scala.collection.mutable.ListBuffer

  object AgentKVDBScope
         extends AgentKVDBNodeScope[String,String,String,String]
         with UUIDOps
         //with BulkCollectDImport
         with Serializable
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type ACTypes = AgentCnxnTypes
    object TheACT extends ACTypes
    override def protoAgentCnxnTypes : ACTypes = TheACT

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes with Serializable
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT
    
    override type MsgTypes = DTSMSHRsrc   
    override type RsrcMsgTypes = DTSMSHRsrc   
    
    @transient
    val protoDreqUUID = getUUID()
    @transient
    val protoDrspUUID = getUUID()    

    @transient
    lazy val aLabel = new CnxnCtxtLeaf[String,String,String]( Left( "a" ) )

    object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
      
      @transient
      override def protoDreq : DReq = MDGetRequest( aLabel )
      @transient
      override def protoDrsp : DRsp = MDGetResponse( aLabel, "" )
      @transient
      override def protoJtsreq : JTSReq =
        JustifiedRequest(
          protoDreqUUID,
          new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
          new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
          getUUID(),
          protoDreq,
          None
        )
      @transient
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
    
    override def protoMsgs : MsgTypes = MonadicDRsrcMsgs
    override def protoRsrcMsgs : RsrcMsgTypes = MonadicDRsrcMsgs

    object Being extends AgentPersistenceScope with Serializable {      
      override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
      object theEMTypes extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
       with Serializable
      {
        case class PrologSubstitution( soln : LinkedHashMap[String,CnxnCtxtLabel[String,String,String]] )
           extends Function1[mTT.Resource,Option[mTT.Resource]] {
             override def apply( rsrc : mTT.Resource ) = {
               Some( mTT.RBoundHM( Some( rsrc ), Some( soln ) ) )
             }
           }
        override type Substitution = PrologSubstitution 
      }      

      override def protoEMTypes : EMTypes =
        theEMTypes      

      object AgentKVDBNodeFactory
             extends BaseAgentKVDBNodeFactoryT
             with AgentKVDBNodeFactoryT
             with WireTap
             with Serializable {                       
        type AgentCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse] = AgentKVDB[ReqBody,RspBody]
        //type AgentNode[Rq <: PersistedKVDBNodeRequest, Rs <: PersistedKVDBNodeResponse] = AgentKVDBNode[Rq,Rs]

        override def tap [A] ( fact : A ) : Unit = {
          BasicLogService.reportage( fact )
        }

        override def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
          here : URI,
          configFileName : Option[String]
        ) : AgentCache[ReqBody,RspBody] = {
          new AgentKVDB[ReqBody, RspBody](
            MURI( here ),
            configFileName
          ) with Blobify with AMQPMonikerOps {          
            override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
              new XStream( new JettisonMappedXmlDriver() ).toXML( x )
            }
            override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
              new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
            }      
            class StringXMLDBManifest(
              override val storeUnitStr : String,
              @transient override val labelToNS : Option[String => String],
              @transient override val textToVar : Option[String => String],
              @transient override val textToTag : Option[String => String],
              @transient override val textToValue: Option[String => String] = Some(identity)
            )
            extends XMLDBManifest( database ) {
              override def valueStorageType : String = {
                throw new Exception( "valueStorageType not overriden in instantiation" )
              }
              override def continuationStorageType : String = {
                throw new Exception( "continuationStorageType not overriden in instantiation" )
              }
              
              override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                cnxn match {
                  case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                  case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
                }           
              } 
              
              def kvNameSpace : String = "record"
              def kvKNameSpace : String = "kRecord"
              
              def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
                ns1.equals( ns2 )
              }
              
              override def asStoreValue(
                rsrc : mTT.Resource
              ) : CnxnCtxtLeaf[String,String,String] with Factual = {
                BasicLogService.tweet(
                  "In asStoreValue on " + this + " for resource: " + rsrc
                )
                val storageDispatch = 
                  rsrc match {
                    case k : mTT.Continuation => {
                      BasicLogService.tweet(
                        "Resource " + rsrc + " is a continuation"
                      )
                      continuationStorageType
                    }
                    case _ => {
                      BasicLogService.tweet(
                        "Resource " + rsrc + " is a value"
                      )
                      valueStorageType
                    }
                  };
                
                BasicLogService.tweet(
                  "storageDispatch: " + storageDispatch
                )
                
                val blob =
                  storageDispatch match {
                    case "Base64" => {
                      val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
                      val oos : ObjectOutputStream = new ObjectOutputStream( baos )
                      oos.writeObject( rsrc.asInstanceOf[Serializable] )
                      oos.close()
                      new String( Base64Coder.encode( baos.toByteArray() ) )
                    }
                    case "CnxnCtxtLabel" => {
                      BasicLogService.tweet(
                        "warning: CnxnCtxtLabel method is using XStream"
                      )
                      toXQSafeJSONBlob( rsrc )                            
                    }
                    case "XStream" => {
                      BasicLogService.tweet(
                        "using XStream method"
                      )
                      
                      toXQSafeJSONBlob( rsrc )
                    }
                    case _ => {
                      throw new Exception( "unexpected value storage type" )
                    }
                  }
                new CnxnCtxtLeaf[String,String,String](
                  Left[String,String]( blob )
                )
              }
              
              def asCacheValue(
                ccl : CnxnCtxtLabel[String,String,String]
              ) : String = {
                BasicLogService.tweet(
                  "converting to cache value"
                )
                ccl match {
                  case CnxnCtxtBranch(
                    "string",
                    CnxnCtxtLeaf( Left( rv ) ) :: Nil
                  ) => {
                    val unBlob =
                      fromXQSafeJSONBlob( rv )
                    
                    unBlob match {
                      case rsrc : mTT.Resource => {
                        getGV( rsrc ).getOrElse( "" )
                      }
                    }
                  }
                  case _ => {
                    //asPatternString( ccl )
                    throw new Exception( "unexpected value form: " + ccl )
                  }
                }
              }

              override def asIndirection(key: mTT.GetRequest, value: Elem): Option[mTT.GetRequest] = ???

              override def asResource(
                key : mTT.GetRequest, // must have the pattern to determine bindings
                value : Elem
              ) : emT.PlaceInstance = {
                val ttt = ( x : String ) => x
                
                val ptn = asPatternString( key )
                //println( "ptn : " + ptn )             
                
                val oRsrc : Option[emT.PlaceInstance] =
                  for(
                    ltns <- labelToNS;
                    ttv <- textToVar;
                    ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
                  ) yield {
                    ccl match {
                      case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
                        val oGvOrK = 
                          (if ( compareNameSpace( ns, kvNameSpace ) ) {     
                            // BUGBUG -- LGM need to return the Solution
                            // Currently the PersistenceManifest has no access to the
                            // unification machinery          
                            
                            for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
                            yield {                                 
                              Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
                            }
                          }
                           else {
                             if ( compareNameSpace( ns, kvKNameSpace ) ) {
                               Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
                             }
                             else {
                               throw new Exception( "unexpected namespace : (" + ns + ")" )
                             }
                           });
                        
                        val cclKey =
                          xmlIfier.fromXML( ltns, ttv, ttt )(
                            xmlIfier.asXML( key )
                          ) match {
                            case Some( cclX ) => cclX
                            case _ => throw new Exception( "xml roundtrip failed " + key )
                          }
                        
                        matchMap( cclKey, k ) match {
                          case Some( soln ) => {
                            emT.PlaceInstance(
                              k,
                              oGvOrK match {
                                case Some( Left( gv ) ) => {
                                  Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
                                }
                                case Some( Right( mTT.Continuation( ks ) ) ) => {
                                  Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
                                }
                                case _ => {
                                  throw new Exception( "excluded middle contract broken: " + oGvOrK )
                                }
                              },
                              // BUGBUG -- lgm : why can't the compiler determine
                              // that this cast is not necessary?
                              theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                            )
                          }
                          case None => {
                            BasicLogService.tweet( "Unexpected matchMap failure: " + cclKey + " " + k )
                            throw new Exception( "matchMap failure " + cclKey + " " + k )
                          }
                        }                                               
                      }
                      case _ => {
                        throw new Exception( "unexpected record format : " + value )
                      }
                    }      
                  }
                
                // BUGBUG -- lgm : this is a job for flatMap
                oRsrc match {
                  case Some( pI ) => {
                    pI
                  }
                  case _ => {
                    throw new Exception( "violated excluded middle : " + oRsrc )
                  }
                }
              }
              
            }
            override def asCacheK(
              ccl : CnxnCtxtLabel[String,String,String]
            ) : Option[mTT.Continuation] = {
              BasicLogService.tweet(
                "converting to cache continuation stack" + ccl
              )
              ccl match {
                case CnxnCtxtBranch(
                  "string",
                  CnxnCtxtLeaf( Left( rv ) ) :: Nil
                ) => {
                  val unBlob =
                    continuationStorageType match {
                      case "CnxnCtxtLabel" => {
                        // BasicLogService.tweet(
                        //                    "warning: CnxnCtxtLabel method is using XStream"
                        //                  )
                        fromXQSafeJSONBlob( rv )
                      }
                      case "XStream" => {
                        fromXQSafeJSONBlob( rv )
                      }
                      case "Base64" => {
                        val data : Array[Byte] = Base64Coder.decode( rv )
//                         val ois : ObjectInputStream =
//                           new ObjectInputStream( new ByteArrayInputStream(  data ) )
                        val ois : DefensiveObjectInputStream =
		                new DefensiveObjectInputStream( new ByteArrayInputStream(  data ) )
                        val o : java.lang.Object = ois.readObject();
                        ois.close()
                        o
                      }
                    }
                  
                  unBlob match {
                    case k : mTT.Resource => {
                      Some( k.asInstanceOf[mTT.Continuation] )
                    }
                    case _ => {
                      throw new Exception(
                        (
                          ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                          + "ill-formatted continuation stack blob : " + rv
                          + "\n" 
                          + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                          + "\n"
                          + "unBlob : " + unBlob
                          + "\n"
                          + "unBlob type : " + unBlob
                          + "\n"
                          + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                        )
                      )
                    }
                  }
                }
                case _ => {
                  throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
                }
              }
            }
            
            override def asCacheK(
              ltns : String => String,
              ttv : String => String,
              value : Elem
            ) : Option[mTT.Continuation] = {
              throw new Exception( "shouldn't be calling this version of asCacheK" )
            }
            override def persistenceManifest : Option[PersistenceManifest] = {
              BasicLogService.tweet(
                (
                  "AgentKVDB : "
                  + "\nthis: " + this
                  + "\n method : persistenceManifest "
                )
              )
              val sid = Some( ( s : String ) => s )
              val kvdb = this;
              Some(
                new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid, sid ) {
                  override def valueStorageType : String = {
                    kvdb.valueStorageType
                  }
                  override def continuationStorageType : String = {
                    kvdb.continuationStorageType
                  }
                }
              )
            }
            def dfStoreUnitStr : String = mnkrExchange( name )
          }
        }
        override def ptToPt[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
          here : URI, there : URI
        )(
          implicit configFileNameOpt : Option[String] 
        ) : AgentKVDBNode[ReqBody,RspBody] = {
          val node =
            new AgentKVDBNode[ReqBody,RspBody](
              mkCache( MURI( here ), configFileNameOpt ),
              List( MURI( there ) ),
              None,
              configFileNameOpt
            ) {
              override def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
                here : URI,
                configFileName : Option[String]
              ) : HashAgentKVDB[ReqBody,RspBody] = {
                BasicLogService.tweet(
                  (
                    "AgentKVDBNode : "
                    + "\nthis: " + this
                    + "\n method : mkInnerCache "
                    + "\n here: " + here
                    + "\n configFileName: " + configFileName
                  )
                )
                new HashAgentKVDB[ReqBody, RspBody](
                  MURI( here ),
                  configFileName
                ) with Blobify with AMQPMonikerOps {            
                  override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                  }
                  override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                  }      
                  class StringXMLDBManifest(
                    override val storeUnitStr : String,
                    @transient override val labelToNS : Option[String => String],
                    @transient override val textToVar : Option[String => String],
                    @transient override val textToTag : Option[String => String],
                    @transient override val textToValue: Option[String => String] = Some(identity)
                  )
                  extends XMLDBManifest( database ) {
                    override def valueStorageType : String = {
                      throw new Exception( "valueStorageType not overriden in instantiation" )
                    }
                    override def continuationStorageType : String = {
                      throw new Exception( "continuationStorageType not overriden in instantiation" )
                    }
                    
                    override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                      cnxn match {
                        case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                        case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
                      }     
                    }   
                    
                    def kvNameSpace : String = "record"
                    def kvKNameSpace : String = "kRecord"
                    
                    def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
                      ns1.equals( ns2 )
                    }
                    
                    override def asStoreValue(
                      rsrc : mTT.Resource
                    ) : CnxnCtxtLeaf[String,String,String] with Factual = {
                      BasicLogService.tweet(
                        "In asStoreValue on " + this + " for resource: " + rsrc
                      )
                      val storageDispatch = 
                        rsrc match {
                          case k : mTT.Continuation => {
                            BasicLogService.tweet(
                              "Resource " + rsrc + " is a continuation"
                            )
                            continuationStorageType
                          }
                          case _ => {
                            BasicLogService.tweet(
                              "Resource " + rsrc + " is a value"
                            )
                            valueStorageType
                          }
                        };
                      
                      BasicLogService.tweet(
                        "storageDispatch: " + storageDispatch
                      )
                      
                      val blob =
                        storageDispatch match {
                          case "Base64" => {
                            val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
                            val oos : ObjectOutputStream = new ObjectOutputStream( baos )
                            oos.writeObject( rsrc.asInstanceOf[Serializable] )
                            oos.close()
                            new String( Base64Coder.encode( baos.toByteArray() ) )
                          }
                          case "CnxnCtxtLabel" => {
                            BasicLogService.tweet(
                              "warning: CnxnCtxtLabel method is using XStream"
                            )
                            toXQSafeJSONBlob( rsrc )                              
                          }
                          case "XStream" => {
                            BasicLogService.tweet(
                              "using XStream method"
                            )
                            
                            toXQSafeJSONBlob( rsrc )
                          }
                          case _ => {
                            throw new Exception( "unexpected value storage type" )
                          }
                        }
                      new CnxnCtxtLeaf[String,String,String](
                        Left[String,String]( blob )
                      )
                    }
                    
                    def asCacheValue(
                      ccl : CnxnCtxtLabel[String,String,String]
                    ) : String = {
                      BasicLogService.tweet(
                        "converting to cache value"
                      )
                      ccl match {
                        case CnxnCtxtBranch(
                          "string",
                          CnxnCtxtLeaf( Left( rv ) ) :: Nil
                        ) => {
                          val unBlob =
                            fromXQSafeJSONBlob( rv )
                          
                          unBlob match {
                            case rsrc : mTT.Resource => {
                              getGV( rsrc ).getOrElse( "" )
                            }
                          }
                        }
                        case _ => {
                          //asPatternString( ccl )
                          throw new Exception( "unexpected value form: " + ccl )
                        }
                      }
                    }

                    override def asIndirection(key: mTT.GetRequest, value: Elem): Option[mTT.GetRequest] = ???

                    override def asResource(
                      key : mTT.GetRequest, // must have the pattern to determine bindings
                      value : Elem
                    ) : emT.PlaceInstance = {
                      val ttt = ( x : String ) => x
                      
                      val ptn = asPatternString( key )
                      //println( "ptn : " + ptn )               
                      
                      val oRsrc : Option[emT.PlaceInstance] =
                        for(
                          ltns <- labelToNS;
                          ttv <- textToVar;
                          ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
                        ) yield {
                          ccl match {
                            case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
                              val oGvOrK = 
                                (if ( compareNameSpace( ns, kvNameSpace ) ) {       
                                  // BUGBUG -- LGM need to return the Solution
                                  // Currently the PersistenceManifest has no access to the
                                  // unification machinery            
                                  
                                  for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
                                  yield {                                   
                                    Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
                                  }
                                }
                                 else {
                                   if ( compareNameSpace( ns, kvKNameSpace ) ) {
                                     Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
                                   }
                                   else {
                                     throw new Exception( "unexpected namespace : (" + ns + ")" )
                                   }
                                 });
                              
                              val cclKey =
                                xmlIfier.fromXML( ltns, ttv, ttt )(
                                  xmlIfier.asXML( key )
                                ) match {
                                  case Some( cclX ) => cclX
                                  case _ => throw new Exception( "xml roundtrip failed " + key )
                                }
                              
                              matchMap( cclKey, k ) match {
                                case Some( soln ) => {
                                  emT.PlaceInstance(
                                    k,
                                    oGvOrK match {
                                      case Some( Left( gv ) ) => {
                                        Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
                                      }
                                      case Some( Right( mTT.Continuation( ks ) ) ) => {
                                        Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
                                      }
                                      case _ => {
                                        throw new Exception( "excluded middle contract broken: " + oGvOrK )
                                      }
                                    },
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                }
                                case None => {
                                  BasicLogService.tweet( "Unexpected matchMap failure: " + cclKey + " " + k )
                                  throw new Exception( "matchMap failure " + cclKey + " " + k )
                                }
                              }                                         
                            }
                            case _ => {
                              throw new Exception( "unexpected record format : " + value )
                            }
                          }      
                        }
                      
                      // BUGBUG -- lgm : this is a job for flatMap
                      oRsrc match {
                        case Some( pI ) => {
                          pI
                        }
                        case _ => {
                          throw new Exception( "violated excluded middle : " + oRsrc )
                        }
                      }
                    }
                    
                  }
                  override def asCacheK(
                    ccl : CnxnCtxtLabel[String,String,String]
                  ) : Option[mTT.Continuation] = {
                    BasicLogService.tweet(
                      "converting to cache continuation stack" + ccl
                    )
                    ccl match {
                      case CnxnCtxtBranch(
                        "string",
                        CnxnCtxtLeaf( Left( rv ) ) :: Nil
                      ) => {
                        val unBlob =
                          continuationStorageType match {
                            case "CnxnCtxtLabel" => {
                              // BasicLogService.tweet(
                              //                      "warning: CnxnCtxtLabel method is using XStream"
                              //                    )
                              fromXQSafeJSONBlob( rv )
                            }
                            case "XStream" => {
                              fromXQSafeJSONBlob( rv )
                            }
                            case "Base64" => {
                              val data : Array[Byte] = Base64Coder.decode( rv )
//                               val ois : ObjectInputStream =
//                                 new ObjectInputStream( new ByteArrayInputStream(  data ) )
                              val ois : DefensiveObjectInputStream =
		                new DefensiveObjectInputStream( new ByteArrayInputStream(  data ) )
                              val o : java.lang.Object = ois.readObject();
                              ois.close()
                              o
                            }
                          }
                        
                        unBlob match {
                          case k : mTT.Resource => {
                            Some( k.asInstanceOf[mTT.Continuation] )
                          }
                          case _ => {
                            throw new Exception(
                              (
                                ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                + "ill-formatted continuation stack blob : " + rv
                                + "\n" 
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                + "\n"
                                + "unBlob : " + unBlob
                                + "\n"
                                + "unBlob type : " + unBlob
                                + "\n"
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              )
                            )
                          }
                        }
                      }
                      case _ => {
                        throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
                      }
                    }
                  }
                  
                  override def asCacheK(
                    ltns : String => String,
                    ttv : String => String,
                    value : Elem
                  ) : Option[mTT.Continuation] = {
                    throw new Exception( "shouldn't be calling this version of asCacheK" )
                  }

                  override def persistenceManifest : Option[PersistenceManifest] = {
                    BasicLogService.tweet(
                      (
                        "HashAgentKVDB : "
                        + "\nthis: " + this
                        + "\n method : persistenceManifest "
                      )
                    )
                    val sid = Some( ( s : String ) => s )
                    val kvdb = this;
                    Some(
                      new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid, sid ) {
                        override def valueStorageType : String = {
                          kvdb.valueStorageType
                        }
                        override def continuationStorageType : String = {
                          kvdb.continuationStorageType
                        }
                      }
                    )
                  }
                  def dfStoreUnitStr : String = mnkrExchange( name )
                }
              }
            }
          spawn {
            node.dispatchDMsgs()
          }
          node
        }
        override def ptToMany[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
          here : URI, there : List[URI]
        )(
          implicit configFileNameOpt : Option[String]
        ) : AgentKVDBNode[ReqBody,RspBody] = {
          val node =
            new AgentKVDBNode[ReqBody,RspBody](
              mkCache( MURI( here ), configFileNameOpt ),
              there.map( MURI( _ ) ),
              None,
              configFileNameOpt
            ) {
              override def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
                here : URI,
                configFileName : Option[String]
              ) : HashAgentKVDB[ReqBody,RspBody] = {
                BasicLogService.tweet(
                  (
                    "AgentKVDBNode : "
                    + "\nthis: " + this
                    + "\n method : mkInnerCache "
                    + "\n here: " + here
                    + "\n configFileName: " + configFileName
                  )
                )
                new HashAgentKVDB[ReqBody, RspBody](
                  MURI( here ),
                  configFileName
                ) with Blobify with AMQPMonikerOps {            
                  override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                  }
                  override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                  }      
                  class StringXMLDBManifest(
                    override val storeUnitStr : String,
                    @transient override val labelToNS : Option[String => String],
                    @transient override val textToVar : Option[String => String],
                    @transient override val textToTag : Option[String => String],
                    @transient override val textToValue: Option[String => String] = Some(identity)
                  )
                  extends XMLDBManifest( database ) {
                    override def valueStorageType : String = {
                      throw new Exception( "valueStorageType not overriden in instantiation" )
                    }
                    override def continuationStorageType : String = {
                      throw new Exception( "continuationStorageType not overriden in instantiation" )
                    }
                    
                    override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                      cnxn match {
                        case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                        case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
                      }     
                    }   
                    
                    def kvNameSpace : String = "record"
                    def kvKNameSpace : String = "kRecord"
                    
                    def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
                      ns1.equals( ns2 )
                    }
                    
                    override def asStoreValue(
                      rsrc : mTT.Resource
                    ) : CnxnCtxtLeaf[String,String,String] with Factual = {
                      BasicLogService.tweet(
                        "In asStoreValue on " + this + " for resource: " + rsrc
                      )
                      val storageDispatch = 
                        rsrc match {
                          case k : mTT.Continuation => {
                            BasicLogService.tweet(
                              "Resource " + rsrc + " is a continuation"
                            )
                            continuationStorageType
                          }
                          case _ => {
                            BasicLogService.tweet(
                              "Resource " + rsrc + " is a value"
                            )
                            valueStorageType
                          }
                        };
                      
                      BasicLogService.tweet(
                        "storageDispatch: " + storageDispatch
                      )
                      
                      val blob =
                        storageDispatch match {
                          case "Base64" => {
                            val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
                            val oos : ObjectOutputStream = new ObjectOutputStream( baos )
                            oos.writeObject( rsrc.asInstanceOf[Serializable] )
                            oos.close()
                            new String( Base64Coder.encode( baos.toByteArray() ) )
                          }
                          case "CnxnCtxtLabel" => {
                            BasicLogService.tweet(
                              "warning: CnxnCtxtLabel method is using XStream"
                            )
                            toXQSafeJSONBlob( rsrc )                              
                          }
                          case "XStream" => {
                            BasicLogService.tweet(
                              "using XStream method"
                            )
                            
                            toXQSafeJSONBlob( rsrc )
                          }
                          case _ => {
                            throw new Exception( "unexpected value storage type" )
                          }
                        }
                      new CnxnCtxtLeaf[String,String,String](
                        Left[String,String]( blob )
                      )
                    }
                    
                    def asCacheValue(
                      ccl : CnxnCtxtLabel[String,String,String]
                    ) : String = {
                      BasicLogService.tweet(
                        "converting to cache value"
                      )
                      ccl match {
                        case CnxnCtxtBranch(
                          "string",
                          CnxnCtxtLeaf( Left( rv ) ) :: Nil
                        ) => {
                          val unBlob =
                            fromXQSafeJSONBlob( rv )
                          
                          unBlob match {
                            case rsrc : mTT.Resource => {
                              getGV( rsrc ).getOrElse( "" )
                            }
                          }
                        }
                        case _ => {
                          //asPatternString( ccl )
                          throw new Exception( "unexpected value form: " + ccl )
                        }
                      }
                    }

                    override def asIndirection(key: mTT.GetRequest, value: Elem): Option[mTT.GetRequest] = ???

                    override def asResource(
                      key : mTT.GetRequest, // must have the pattern to determine bindings
                      value : Elem
                    ) : emT.PlaceInstance = {
                      val ttt = ( x : String ) => x
                      
                      val ptn = asPatternString( key )
                      //println( "ptn : " + ptn )                     

                      val oRsrc : Option[emT.PlaceInstance] =
                        for(
                          ltns <- labelToNS;
                          ttv <- textToVar;
                          ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
                        ) yield {
                          ccl match {
                            case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
                              val oGvOrK = 
                                (if ( compareNameSpace( ns, kvNameSpace ) ) {       
                                  // BUGBUG -- LGM need to return the Solution
                                  // Currently the PersistenceManifest has no access to the
                                  // unification machinery            
                                  
                                  for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
                                  yield {                                   
                                    Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
                                  }
                                }
                                 else {
                                   if ( compareNameSpace( ns, kvKNameSpace ) ) {
                                     Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
                                   }
                                   else {
                                     throw new Exception( "unexpected namespace : (" + ns + ")" )
                                   }
                                 });
                              
                              val cclKey =
                                xmlIfier.fromXML( ltns, ttv, ttt )(
                                  xmlIfier.asXML( key )
                                ) match {
                                  case Some( cclX ) => cclX
                                  case _ => throw new Exception( "xml roundtrip failed " + key )
                                }

                              BasicLogService.tweet( "******************* asResource *********************" )
                              BasicLogService.tweet( "cclKey : " + cclKey )
                              BasicLogService.tweet( "k : " + k )
                              BasicLogService.tweet( "******************* asResource *********************" )
                              
                              matchMap( cclKey, k ) match {
                                case Some( soln ) => {
                                  emT.PlaceInstance(
                                    k,
                                    oGvOrK match {
                                      case Some( Left( gv ) ) => {
                                        Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
                                      }
                                      case Some( Right( mTT.Continuation( ks ) ) ) => {
                                        Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
                                      }
                                      case _ => {
                                        throw new Exception( "excluded middle contract broken: " + oGvOrK )
                                      }
                                    },
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                }
                                case None => {
                                  BasicLogService.tweet( "Unexpected matchMap failure: " + cclKey + " " + k )
                                  throw new Exception( "matchMap failure " + cclKey + " " + k )
                                }
                              }                                         
                            }
                            case _ => {
                              throw new Exception( "unexpected record format : " + value )
                            }
                          }      
                        }
                      
                      // BUGBUG -- lgm : this is a job for flatMap
                      oRsrc match {
                        case Some( pI ) => {
                          pI
                        }
                        case _ => {
                          throw new Exception( "violated excluded middle : " + oRsrc )
                        }
                      }
                    }
                    
                  }
                  override def asCacheK(
                    ccl : CnxnCtxtLabel[String,String,String]
                  ) : Option[mTT.Continuation] = {
                    BasicLogService.tweet(
                      "converting to cache continuation stack" + ccl
                    )
                    ccl match {
                      case CnxnCtxtBranch(
                        "string",
                        CnxnCtxtLeaf( Left( rv ) ) :: Nil
                      ) => {
                        val unBlob =
                          continuationStorageType match {
                            case "CnxnCtxtLabel" => {
                              // BasicLogService.tweet(
                              //                      "warning: CnxnCtxtLabel method is using XStream"
                              //                    )
                              fromXQSafeJSONBlob( rv )
                            }
                            case "XStream" => {
                              fromXQSafeJSONBlob( rv )
                            }
                            case "Base64" => {
                              val data : Array[Byte] = Base64Coder.decode( rv )
//                               val ois : ObjectInputStream =
//                                 new ObjectInputStream( new ByteArrayInputStream(  data ) )
                              val ois : DefensiveObjectInputStream =
		                new DefensiveObjectInputStream( new ByteArrayInputStream(  data ) )
                              val o : java.lang.Object = ois.readObject();
                              ois.close()
                              o
                            }
                          }
                        
                        unBlob match {
                          case k : mTT.Resource => {
                            Some( k.asInstanceOf[mTT.Continuation] )
                          }
                          case _ => {
                            throw new Exception(
                              (
                                ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                + "ill-formatted continuation stack blob : " + rv
                                + "\n" 
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                + "\n"
                                + "unBlob : " + unBlob
                                + "\n"
                                + "unBlob type : " + unBlob
                                + "\n"
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              )
                            )
                          }
                        }
                      }
                      case _ => {
                        throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
                      }
                    }
                  }
                  
                  override def asCacheK(
                    ltns : String => String,
                    ttv : String => String,
                    value : Elem
                  ) : Option[mTT.Continuation] = {
                    throw new Exception( "shouldn't be calling this version of asCacheK" )
                  }
                  override def persistenceManifest : Option[PersistenceManifest] = {
                    BasicLogService.tweet(
                      (
                        "HashAgentKVDB : "
                        + "\nthis: " + this
                        + "\n method : persistenceManifest "
                      )
                    )
                    val sid = Some( ( s : String ) => s )
                    val kvdb = this;
                    Some(
                      new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid, sid ) {
                        override def valueStorageType : String = {
                          kvdb.valueStorageType
                        }
                        override def continuationStorageType : String = {
                          kvdb.continuationStorageType
                        }
                      }
                    )
                  }
                  def dfStoreUnitStr : String = mnkrExchange( name )
                }
              }
            }
          spawn {
            BasicLogService.tweet( "initiating dispatch on " + node )
            node.dispatchDMsgs()
          }
          node
        }
        def loopBack[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
          here : URI
        )(
          implicit configFileNameOpt : Option[String]
        ) : AgentKVDBNode[ReqBody,RspBody] = {
          val exchange = uriExchange( here )
          val hereNow =
            new URI(
              here.getScheme,
              here.getUserInfo,
              here.getHost,
              here.getPort,
              "/" + exchange + "Local",
              here.getQuery,
              here.getFragment
            )
          val thereNow =
            new URI(
              here.getScheme,
              here.getUserInfo,
              here.getHost,
              here.getPort,
              "/" + exchange + "Remote",
              here.getQuery,
              here.getFragment
            )       
          
          val node =
            new AgentKVDBNode[ReqBody, RspBody](
              mkCache( MURI( hereNow ), configFileNameOpt ),
              List( MURI( thereNow ) ),
              None,
              configFileNameOpt
            ) {
              override def mkInnerCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
                here : URI,
                configFileName : Option[String]
              ) : HashAgentKVDB[ReqBody,RspBody] = {
                BasicLogService.tweet(
                  (
                    "AgentKVDBNode : "
                    + "\nthis: " + this
                    + "\n method : mkInnerCache "
                    + "\n here: " + here
                    + "\n configFileName: " + configFileName
                  )
                )
                new HashAgentKVDB[ReqBody, RspBody](
                  MURI( here ),
                  configFileName
                ) with Blobify with AMQPMonikerOps {            
                  override def toXQSafeJSONBlob( x : java.lang.Object ) : String = {
                    new XStream( new JettisonMappedXmlDriver() ).toXML( x )
                  }
                  override def fromXQSafeJSONBlob( blob : String ) : java.lang.Object = {              
                    new XStream( new JettisonMappedXmlDriver() ).fromXML( blob )
                  }      
                  class StringXMLDBManifest(
                    override val storeUnitStr : String,
                    @transient override val labelToNS : Option[String => String],
                    @transient override val textToVar : Option[String => String],
                    @transient override val textToTag : Option[String => String],
                    @transient override val textToValue: Option[String => String] = Some(identity)
                  )
                  extends XMLDBManifest( database ) {
                    override def valueStorageType : String = {
                      throw new Exception( "valueStorageType not overriden in instantiation" )
                    }
                    override def continuationStorageType : String = {
                      throw new Exception( "continuationStorageType not overriden in instantiation" )
                    }
                    
                    override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
                      cnxn match {
                        case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
                        case acT.AgentCnxn( s, l, t ) => s.getHost + l.toString + t.getHost
                      }     
                    }   
                    
                    def kvNameSpace : String = "record"
                    def kvKNameSpace : String = "kRecord"
                    
                    def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
                      ns1.equals( ns2 )
                    }
                    
                    override def asStoreValue(
                      rsrc : mTT.Resource
                    ) : CnxnCtxtLeaf[String,String,String] with Factual = {
                      BasicLogService.tweet(
                        "In asStoreValue on " + this + " for resource: " + rsrc
                      )
                      val storageDispatch = 
                        rsrc match {
                          case k : mTT.Continuation => {
                            BasicLogService.tweet(
                              "Resource " + rsrc + " is a continuation"
                            )
                            continuationStorageType
                          }
                          case _ => {
                            BasicLogService.tweet(
                              "Resource " + rsrc + " is a value"
                            )
                            valueStorageType
                          }
                        };
                      
                      BasicLogService.tweet(
                        "storageDispatch: " + storageDispatch
                      )
                      
                      val blob =
                        storageDispatch match {
                          case "Base64" => {
                            val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
                            val oos : ObjectOutputStream = new ObjectOutputStream( baos )
                            oos.writeObject( rsrc.asInstanceOf[Serializable] )
                            oos.close()
                            new String( Base64Coder.encode( baos.toByteArray() ) )
                          }
                          case "CnxnCtxtLabel" => {
                            BasicLogService.tweet(
                              "warning: CnxnCtxtLabel method is using XStream"
                            )
                            toXQSafeJSONBlob( rsrc )                              
                          }
                          case "XStream" => {
                            BasicLogService.tweet(
                              "using XStream method"
                            )
                            
                            toXQSafeJSONBlob( rsrc )
                          }
                          case _ => {
                            throw new Exception( "unexpected value storage type" )
                          }
                        }
                      new CnxnCtxtLeaf[String,String,String](
                        Left[String,String]( blob )
                      )
                    }
                    
                    def asCacheValue(
                      ccl : CnxnCtxtLabel[String,String,String]
                    ) : String = {
                      BasicLogService.tweet(
                        "converting to cache value"
                      )
                      ccl match {
                        case CnxnCtxtBranch(
                          "string",
                          CnxnCtxtLeaf( Left( rv ) ) :: Nil
                        ) => {
                          val unBlob =
                            fromXQSafeJSONBlob( rv )
                          
                          unBlob match {
                            case rsrc : mTT.Resource => {
                              getGV( rsrc ).getOrElse( "" )
                            }
                          }
                        }
                        case _ => {
                          //asPatternString( ccl )
                          throw new Exception( "unexpected value form: " + ccl )
                        }
                      }
                    }

                    override def asIndirection(key: mTT.GetRequest, value: Elem): Option[mTT.GetRequest] = ???

                    override def asResource(
                      key : mTT.GetRequest, // must have the pattern to determine bindings
                      value : Elem
                    ) : emT.PlaceInstance = {
                      val ttt = ( x : String ) => x
                      
                      val ptn = asPatternString( key )
                                            
                      val oRsrc : Option[emT.PlaceInstance] =
                        for(
                          ltns <- labelToNS;
                          ttv <- textToVar;
                          ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
                        ) yield {
                          ccl match {
                            case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
                              val oGvOrK = 
                                (if ( compareNameSpace( ns, kvNameSpace ) ) {       
                                  // BUGBUG -- LGM need to return the Solution
                                  // Currently the PersistenceManifest has no access to the
                                  // unification machinery            
                                  
                                  for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
                                  yield {                                   
                                    Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
                                  }
                                }
                                 else {
                                   if ( compareNameSpace( ns, kvKNameSpace ) ) {
                                     Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
                                   }
                                   else {
                                     throw new Exception( "unexpected namespace : (" + ns + ")" )
                                   }
                                 });
                              
                              val cclKey =
                                xmlIfier.fromXML( ltns, ttv, ttt )(
                                  xmlIfier.asXML( key )
                                ) match {
                                  case Some( cclX ) => cclX
                                  case _ => throw new Exception( "xml roundtrip failed " + key )
                                }
                              
                              matchMap( cclKey, k ) match {
                                case Some( soln ) => {
                                  emT.PlaceInstance(
                                    k,
                                    oGvOrK match {
                                      case Some( Left( gv ) ) => {
                                        Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
                                      }
                                      case Some( Right( mTT.Continuation( ks ) ) ) => {
                                        Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
                                      }
                                      case _ => {
                                        throw new Exception( "excluded middle contract broken: " + oGvOrK )
                                      }
                                    },
                                    // BUGBUG -- lgm : why can't the compiler determine
                                    // that this cast is not necessary?
                                    theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
                                  )
                                }
                                case None => {
                                  BasicLogService.tweet( "Unexpected matchMap failure: " + cclKey + " " + k )
                                  throw new Exception( "matchMap failure " + cclKey + " " + k )
                                }
                              }                                         
                            }
                            case _ => {
                              throw new Exception( "unexpected record format : " + value )
                            }
                          }      
                        }
                      
                      // BUGBUG -- lgm : this is a job for flatMap
                      oRsrc match {
                        case Some( pI ) => {
                          pI
                        }
                        case _ => {
                          throw new Exception( "violated excluded middle : " + oRsrc )
                        }
                      }
                    }
                    
                  }
                  override def asCacheK(
                    ccl : CnxnCtxtLabel[String,String,String]
                  ) : Option[mTT.Continuation] = {
                    BasicLogService.tweet(
                      "converting to cache continuation stack" + ccl
                    )
                    ccl match {
                      case CnxnCtxtBranch(
                        "string",
                        CnxnCtxtLeaf( Left( rv ) ) :: Nil
                      ) => {
                        val unBlob =
                          continuationStorageType match {
                            case "CnxnCtxtLabel" => {
                              // BasicLogService.tweet(
                              //                      "warning: CnxnCtxtLabel method is using XStream"
                              //                    )
                              fromXQSafeJSONBlob( rv )
                            }
                            case "XStream" => {
                              fromXQSafeJSONBlob( rv )
                            }
                            case "Base64" => {
                              val data : Array[Byte] = Base64Coder.decode( rv )
//                               val ois : ObjectInputStream =
//                                 new ObjectInputStream( new ByteArrayInputStream(  data ) )
                              val ois : DefensiveObjectInputStream =
		                new DefensiveObjectInputStream( new ByteArrayInputStream(  data ) )
                              val o : java.lang.Object = ois.readObject();
                              ois.close()
                              o
                            }
                          }
                        
                        unBlob match {
                          case k : mTT.Resource => {
                            Some( k.asInstanceOf[mTT.Continuation] )
                          }
                          case _ => {
                            throw new Exception(
                              (
                                ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                + "ill-formatted continuation stack blob : " + rv
                                + "\n" 
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                                + "\n"
                                + "unBlob : " + unBlob
                                + "\n"
                                + "unBlob type : " + unBlob
                                + "\n"
                                + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
                              )
                            )
                          }
                        }
                      }
                      case _ => {
                        throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
                      }
                    }
                  }
                  
                  override def asCacheK(
                    ltns : String => String,
                    ttv : String => String,
                    value : Elem
                  ) : Option[mTT.Continuation] = {
                    throw new Exception( "shouldn't be calling this version of asCacheK" )
                  }
                  override def persistenceManifest : Option[PersistenceManifest] = {
                    BasicLogService.tweet(
                      (
                        "HashAgentKVDB : "
                        + "\nthis: " + this
                        + "\n method : persistenceManifest "
                      )
                    )
                    val sid = Some( ( s : String ) => s )
                    val kvdb = this;
                    Some(
                      new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid, sid ) {
                        override def valueStorageType : String = {
                          kvdb.valueStorageType
                        }
                        override def continuationStorageType : String = {
                          kvdb.continuationStorageType
                        }
                      }
                    )
                  }
                  def dfStoreUnitStr : String = mnkrExchange( name )
                }
              }
            }
          spawn {
            BasicLogService.tweet( "initiating dispatch on " + node )
            node.dispatchDMsgs()
          }
          node
        }
      }
    }

    import java.util.regex.{Pattern => RegexPtn, Matcher => RegexMatcher}
    
    def createExchange[Rsrc]() : MonadicTupleSpace[String,String,Rsrc] = {
      new MonadicTupleSpace[String,String,Rsrc] with WireTap
        with ConfigurationTrampoline {

          override type Substitution = IdentitySubstitution

          override val theMeetingPlace = new HashMap[String,Rsrc]()
          override val theChannels = new HashMap[String,Rsrc]()
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
            if ( fits( ptn, place ) ) {
              Some( IdentitySubstitution() )
            }
            else {
              None
            }
          }
          
        }      
    }

    def handleEntry( grndTypeStr : String, imprtTypeStr : String, mTTTypeStr : String )( json : JValue, acc : Buffer[Elem] ) : Buffer[Elem] = {
      for(
        JObject( fvs ) <- json \\ "putval" ;
        JArray( valueArray ) <- json \\ "values" ;
        JArray( dstypes ) <- json \\ "dstypes" ;
        JArray( dsnames ) <- json \\ "dsnames" ;
        JDouble( time ) <- json \\ "time" ;
        JDouble( interval ) <- json \\ "interval" ; 
        JString( host ) <- json \\ "host" ; 
        JString( plugin ) <- json \\ "plugin" ;
        JString( plugin_instance ) <- json \\ "plugin_instance" ;
        JString( cdtype ) <- json \\ "type" ;
        JString( type_instance ) <- json \\ "type_instance"
      ) {
        val gvStr =
          new XStream(
            new JettisonMappedXmlDriver()
          ).toXML( UUIDWrapper( ( getUUID + "" ) ) ).replace(
            "com.biosimilarity.lift.lib.bulk.BulkCollectDImport$UUIDWrapper",
            grndTypeStr
          ).replace(
            imprtTypeStr,
            mTTTypeStr
          ).replace(
            "youyouid",
            "v"
          )
        
        acc +=
        <record>
          <comProtegraAgentservicesstorePutVal>
            <values>{for( JInt( v ) <- valueArray ) yield {<string>{v}</string>}}</values>
            <dstypes>{for( JString( t ) <- dstypes ) yield {<string>{t}</string>}}</dstypes>
            <dsnames>{for( JString( n ) <- dsnames ) yield {<string>{n}</string>}}</dsnames>
            <time>{<string>{time}</string>}</time>
            <interval>{<string>{interval}</string>}</interval>
            <host>{<string>{host}</string>}</host>
            <plugin>{<string>{plugin}</string>}</plugin>
            <plugin_instance>{<string>{plugin_instance}</string>}</plugin_instance>
            <type>{<string>{cdtype}</string>}</type>
            <type_instance>{<string>{type_instance}</string>}</type_instance>
          </comProtegraAgentservicesstorePutVal>
          <string>{gvStr}</string>
        </record>
      }
      acc
    }

    def handleEntry( json : JValue, acc : Buffer[Elem] ) : Buffer[Elem] = {
      handleEntry(
        "com.biosimilarity.lift.model.store.MonadicTermTypes$Ground",
        "com.protegra.agentservicestore.usage.BulkCollectDImporter",
        "com.protegra.agentservicestore.usage.AgentKVDBScope$TheMTT"
      )(
        json, acc
      )
    }
   
    @transient
    lazy val testRunID = getUUID
    @transient
    var runNum = 0

    @transient
    implicit lazy val pvOne =
      PutVal(
        List( 558815, 43649779 ),
        List( "derive", "derive" ),
        List( "rx", "tx" ),
        1334349094.633,
        10.000,
        "server-75530.localdomain",
        "interface",
        "eth0",
        "if_octets",
        ""
      )
    @transient
       implicit lazy val pvTwo =
         PutVal(
        List( 558815, 43649779 ),
        List( "derive", "derive" ),
        List( "rx", "tx" ),
        1334349094.633,
        10.000,
        "jason.protegra",
        "interface",
        "eth0",
        "if_octets",
        ""
         )
    @transient
    lazy val entryStr = 
      """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """

    @transient
    implicit lazy val entryStream : Stream[String] =
      ( List( entryStr ) ).toStream append ( entryStream map { ( s : String ) => /* print( "." );*/ s } )    

    @transient
    lazy val rcrdStream : Stream[Elem] = {
      val acc = new ListBuffer[Elem]()
      val rcrd = handleEntry( parse( entryStr ), acc )( 0 )
      ( List( rcrd ) ).toStream append ( rcrdStream map { ( rcrd : Elem ) => /* print( "." ); */ rcrd } )
    }

    def readEntries(
      host : String, queue : String, file : String, lthrd : String, dbChunk : Int
    )( implicit eStrm : Stream[String] ) : String = {      
            
      val acc = rcrdStream.take( dbChunk )

      val rcrdsFileNameSfx = "" + getUUID + "-" + runNum + ""
      val recordsFileName = ( file + rcrdsFileNameSfx + ".xml" )
      val db = <records>{acc.toList}</records>

      BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
      BasicLogService.tweet( "\nsaving a chunk of records ( " + dbChunk + " ) to " + recordsFileName )
      BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )

      //scala.xml.XML.saveFull( recordsFileName, db, "UTF-8", true, null )

      recordsFileName
    }

    def importData( lthrd : String, chunkSize : Int )( implicit eStrm : Stream[String] ) : List[String] = {            
      List(
        readEntries( "localhost", "collectDSample", "collectDImport", lthrd, chunkSize )( eStrm )
      )
    }

    def loadData( numOfEntries : Int ) : Unit = {
      // Nothing to do
    }

    @transient
    val entryExchange : MonadicTupleSpace[String,String,String] = createExchange[String]()
    @transient
    val dataChunkExchange : MonadicTupleSpace[String,String,ListBuffer[Elem]] = createExchange[ListBuffer[Elem]]()

    def supplyEntries( host : String, queue : String, numOfEntries : Int ) : Unit = {
      // create an AMQP scope
      val collectDAMQPScope = new AMQPStdScope[String]()
      // create an AMQP Queue monad
      val collectDQM =
        new collectDAMQPScope.AMQPQueueHostExchangeM[String](
          host,
          queue
        )
      // get an empty queue
      val collectDQ = collectDQM.zero[String]
      BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
      BasicLogService.tweet( "creating entries" )
      BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )

      val entry = """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """

      lazy val entryStream : Stream[String] =
        ( List( entry ) ).toStream append ( entryStream map { ( s : String ) => print( "." ); s } )

      for( i <- 1 to numOfEntries ) { collectDQ ! entryStream( i ) }

      BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
      BasicLogService.tweet( "\nentries created" )
      BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
    }

    def readEntriesFromRabbitMQ(
      host : String, queue : String, file : String, lthrd : String, dbChunk : Int
    ) : ListBuffer[String] = {
      // create an AMQP scope
      val collectDAMQPScope = new AMQPStdScope[String]()
    // create an AMQP Queue monad
      val collectDQM =
        new collectDAMQPScope.AMQPQueueHostExchangeM[String](
          host,
          queue
        )
      // get an empty queue
      val collectDQ = collectDQM.zero[String]    
      
      val acc = new ListBuffer[Elem]()      
      val fileNames = new ListBuffer[String]()      
      val lock = new Lock()

      BasicLogService.tweet( "reading entries" )
      for ( entry <- collectDQM( collectDQ ) ) {
        print( "." )
        handleEntry( parse( entry ), acc )
        lock.acquire
        if ( acc.size >= dbChunk ) {
          runNum += 1
          val rcrdsFileNameSfx = "" + testRunID + runNum + ""
          val recordsFileName = ( file + rcrdsFileNameSfx + ".xml" )        
          val db = <records>{acc.toList}</records>
          
          BasicLogService.tweet( "\nsaving a chunk of records ( " + dbChunk + " ) to " + recordsFileName )
          //scala.xml.XML.saveFull( recordsFileName, db, "UTF-8", true, null )
          fileNames += recordsFileName

          BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
          BasicLogService.tweet( "putting @ " + ( lthrd + "_" + recordsFileName ) )
          BasicLogService.tweet( "to trigger adding the data file to the kvdb node db" )
          BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )

          reset {
            entryExchange.putS( lthrd + "_" + recordsFileName, rcrdsFileNameSfx )

            BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
            BasicLogService.tweet( "waiting @ " + ( lthrd + rcrdsFileNameSfx ) )
            BasicLogService.tweet( "to be able to continue processing json entries" )
            BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )

            for( rsrc <- entryExchange.getS( lthrd + rcrdsFileNameSfx ) ) {

              BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )
              BasicLogService.tweet( "got " + rsrc + " @ " + lthrd + rcrdsFileNameSfx )
              BasicLogService.tweet( "---------------------------******>>>>>>******---------------------------" )

              rsrc match {
                case Some( msg ) => {
                  BasicLogService.tweet( "found the droids we were looking for. " + msg )
                }
                case _ => {
                  throw new Exception( "unexpected communication: " + rsrc )
                }
              }

            }
          }
          acc.clear
        }
        lock.release    
      }
      BasicLogService.tweet( "\nentries read" )
      fileNames
    }

    def loadDataToRabbitMQ( numOfEntries : Int ) : Unit = {
      supplyEntries( "localhost", "collectDSample", numOfEntries )
    }
    // def importDataFromRabbitMQ( lthrd : String, chunkSize : Int ) : List[String] = {            
//       val fileNameRoot = "collectDImport"
//       val filePtn = ( fileNameRoot + ".*" )
//       val lb = readEntriesFromRabbitMQ( "localhost", "collectDSample", "collectDImport", lthrd, chunkSize )
//       val s = new scala.collection.mutable.HashSet[String]( )

//       def loop( s : scala.collection.mutable.HashSet[String], n : Int ) : Unit = {
//      println( "---------------------------******>>>>>>******---------------------------" )
//      println( "waiting @ " + ( lthrd + "_" + filePtn ) )
//      println( "to add a filename to the queue for adding to kvdb node db" )
//      println( "---------------------------******>>>>>>******---------------------------" )

//      reset {
//        for( rsrc <- entryExchange.getS( lthrd + "_" + filePtn ) ) {
//          println( "---------------------------******>>>>>>******---------------------------" )
//          println( "got " + rsrc + " @ " + lthrd + "_" + filePtn )
//          println( "---------------------------******>>>>>>******---------------------------" )
//          rsrc match {
//            case Some( fileNameSfx ) => {
//              val fileName = fileNameRoot + fileNameSfx + ".xml"
                
//              println( "---------------------------******>>>>>>******---------------------------" )
//              println( "alerted that " + fileName + ".xml" + " has been written." )
//              println( "---------------------------******>>>>>>******---------------------------" )
                
//              s += fileName         
                
//              println( "---------------------------******>>>>>>******---------------------------" )
//              println( "putting @ " + ( lthrd + fileNameSfx ) )
//              println( "---------------------------******>>>>>>******---------------------------" )
                
//              entryExchange.putS( lthrd + fileNameSfx, "move along." );
                
//              if ( s.size < n ) { loop( s, n ) }
                
//              () // to keep the type checker happy
//            }
//            case _ => {
//              throw new Exception( "shouldn't be in this case" )
//            }
//          };
//          ()
//        }
//      }
//       }

//       loop( s, 1 )

//       s.toList
//     }

  }

  object TestConfigurationDefaults {
    val localHost : String = "localhost"
    val localPort : Int = 5672
    val remoteHost : String = "localhost"
    val remotePort : Int = 5672
    val dataLocation : String = "/cnxnTestProtocol"
    val numEntriesSeed : Int = 1000
    val chunkSizeSeed : Int = 250
    val numEntriesFloor : Int = 250
    val chunkSizeFloor : Int = 250
    val numNodesSeed : Int = 10
    val numCnxnsSeed : Int = 100
    val nodesFloor : Int = 1
    val cnxnsFloor : Int = 10
  }

  trait TestGenerationConfiguration extends ConfigurationTrampoline {
    def localHost : String =
      configurationFromFile.get( "localHost" ).getOrElse( bail() )
    def localPort : Int =
      configurationFromFile.get( "localPort" ).getOrElse( bail() ).toInt
    def remoteHost : String =
      configurationFromFile.get( "remoteHost" ).getOrElse( bail() )
    def remotePort : Int =
      configurationFromFile.get( "remotePort" ).getOrElse( bail() ).toInt
    def dataLocation : String = 
      configurationFromFile.get( "dataLocation" ).getOrElse( bail() )
    def numEntriesSeed : Int = 
      configurationFromFile.get( "numEntriesSeed" ).getOrElse( bail() ).toInt
    def chunkSizeSeed : Int = 
      configurationFromFile.get( "chunkSizeSeed" ).getOrElse( bail() ).toInt
    def numEntriesFloor : Int = 
      configurationFromFile.get( "numEntriesFloor" ).getOrElse( bail() ).toInt
    def chunkSizeFloor : Int = 
      configurationFromFile.get( "chunkSizeFloor" ).getOrElse( bail() ).toInt
    def numNodesSeed : Int = 
      configurationFromFile.get( "numNodesSeed" ).getOrElse( bail() ).toInt
    def numCnxnsSeed : Int = 
      configurationFromFile.get( "numCnxnsSeed" ).getOrElse( bail() ).toInt
    def nodesFloor : Int = 
      configurationFromFile.get( "nodesFloor" ).getOrElse( bail() ).toInt
    def cnxnsFloor : Int = 
      configurationFromFile.get( "cnxnsFloor" ).getOrElse( bail() ).toInt
  }

  case class AgentUseCase( override val configFileName : Option[String] )
       extends TestGenerationConfiguration {
    import AgentKVDBScope._
    import Being._
    import AgentKVDBNodeFactory._

    import CnxnConversionStringScope._

    import com.protegra_ati.agentservices.store.extensions.StringExtensions._

    import org.basex.core._
    import org.basex.core.cmd.Open
    import org.basex.core.cmd.Add
    import org.basex.core.cmd.CreateDB

    //override def configFileName : Option[String] = None
    override def configurationDefaults : ConfigurationDefaults = {
      TestConfigurationDefaults.asInstanceOf[ConfigurationDefaults]
    }

    val cnxnGlobal = new acT.AgentCnxn("Global".toURI, "", "Global".toURI)

    def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      dataLocation : String,
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])] = {
      val ( localExchange, remoteExchange ) = 
        if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
          ( dataLocation, dataLocation + "Remote" )       
        }
        else {
          ( dataLocation, dataLocation )          
        }

      if ( returnTwist ) {
        Right[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])](
          (
            ptToPt[ReqBody, RspBody](
              new URI( "agent", null, localHost, localPort, localExchange, null, null ),
              new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
            ),
            ptToPt[ReqBody, RspBody](         
              new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
              new URI( "agent", null, localHost, localPort, localExchange, null, null )
            )
          )
        )
      }
      else {
        Left[Being.AgentKVDBNode[ReqBody, RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])](
          ptToPt(
            new URI( "agent", null, localHost, localPort, localExchange, null, null ),
            new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
          )
        )
      }
    }

    def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])] = {
      setup( "/agentUseCaseProtocol", localHost, localPort, remoteHost, remotePort )
    }

    def agent[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( 
      dataLocation : String
    ) : Being.AgentKVDBNode[ReqBody,RspBody] = {
      val Right( ( client, server ) ) = 
        setup[ReqBody,RspBody](
          dataLocation, "localhost", 5672, "localhost", 5672
        )( true )
      client
    }

    // def setupTestData[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
//       numOfEntries : Int, chunkSize : Int
//     ) : scala.collection.Map[String,Being.AgentKVDBNode[ReqBody,RspBody]]
//     = {
//       val currentDir = new java.io.File(".").getAbsolutePath()
//       val rslt = new HashMap[String,Being.AgentKVDBNode[ReqBody,RspBody]]()
//       val lthrd = AgentKVDBScope.getUUID + ""

//       AgentKVDBScope.loadData( numOfEntries )
//       for(   
//      recordsFileName <- importData( lthrd, chunkSize ); 
//      recordsFileNameRoot = recordsFileName.replace( ".xml", "" );
//      recordsFullFileName = currentDir.replace( "/.", "/" + recordsFileName );
//      node = agent[ReqBody,RspBody]( "/" + ( "prebuiltCnxnProtocol" ) );
//      clientSession = node.cache.clientSessionFromConfig;
//      cnxn = new acT.AgentCnxn( recordsFileNameRoot.toURI, "", recordsFileNameRoot.toURI );
//      nodePart = node.getLocalPartition( cnxn )
//       ) {
//      val dbName =
//        nodePart.cache.persistenceManifest match {
//          case Some( pd ) => pd.storeUnitStr( cnxn )
//          case None => throw new Exception( "missing persistence manifest" )
//        }
        
//      clientSession.execute( new CreateDB( dbName ) )
//      clientSession.execute( new Add( recordsFullFileName ) )
//      rslt += ( recordsFullFileName -> node )
//       }        
//       rslt
//     }

    def fileNameToCnxn( fileName : String ) : acT.AgentCnxn = {
      val fileNameRoot = fileName.split( '/' ).last
      new acT.AgentCnxn( fileNameRoot.toURI, "", fileNameRoot.toURI )
    }

    def tStream[T]( seed : T )( fresh : T => T ) : Stream[T] = {
      lazy val loopStrm : Stream[T] =
        ( List( seed ) ).toStream append ( loopStrm map fresh );
      loopStrm
    }

    def uriStream( seed : URI )( fresh : URI => URI ) : Stream[URI] = {
      tStream[URI]( seed )( fresh )
    }
    def agentURIStream( seed : URI ) : Stream[URI] = {
      def fresh( uri : URI ) : URI = {
        val ( scheme, host, port, path ) =
          ( uri.getScheme, uri.getHost, uri.getPort, uri.getPath );
        val spath = path.split( '/' )
        val exchange =
          spath.length match {
            case 2 => spath( 1 )
            case m : Int if m > 2 => spath( 1 )
            case n : Int if n < 2 => "defaultAgentExchange_0"
          }
        val sExch = exchange.split( '_' )
        val ( exchRoot, exchCount ) =
          sExch.length match {
            case 2 => { ( sExch( 0 ), sExch( 1 ).toInt ) }
            case 1 => { ( sExch( 0 ), 0 ) }
            case n : Int if n > 2 => {
              ( ( "" /: sExch )( _ + "_" + _ ), sExch( sExch.length - 1 ).toInt )
            }
          }
        val nExchCount = exchCount + 1

        new URI( scheme, null, host, port, exchRoot + nExchCount, null, null )
      }
      uriStream( seed )( fresh )
    }

    def kvdbStream[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Stream[Either[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])]] = {
      val ( localExchange, remoteExchange ) = 
        if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
          ( "/agentUseCaseProtocolLocal_0", "/agentUseCaseProtocolRemote_0" )     
        }
        else {
          ( "/agentUseCaseProtocol_0", "/agentUseCaseProtocol_0" )        
        }

      val localURI = new URI( "agent", null, localHost, localPort, localExchange, null, null )
      val remoteURI = new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
      val localURIStrm = agentURIStream( localURI )
      val remoteURIStrm = agentURIStream( remoteURI )
      val lrURIStrm = localURIStrm zip remoteURIStrm

      if ( returnTwist ) {              
        lrURIStrm map {
          ( lrURI : ( URI, URI ) ) =>
            Right[Being.AgentKVDBNode[ReqBody,RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])](
              (
                ptToPt[ReqBody,RspBody]( lrURI._1, lrURI._2 ),
                ptToPt[ReqBody,RspBody]( lrURI._2, lrURI._1 )
              )
            )
        }
      }
      else {
        lrURIStrm map {
          ( lrURI : ( URI, URI ) ) =>
            Left[Being.AgentKVDBNode[ReqBody, RspBody],(Being.AgentKVDBNode[ReqBody, RspBody],Being.AgentKVDBNode[ReqBody, RspBody])](
              ptToPt[ReqBody,RspBody]( lrURI._1, lrURI._2 )
            )
        }       
      }
    }

//     def cnxnStream[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
//       node : Being.AgentKVDBNode[ReqBody,RspBody],
//       numOfEntries : Int,
//       chunkSize : Int
//     ) : Stream[acT.AgentCnxn] = {
//       val currentDir = new java.io.File(".").getAbsolutePath()
//       val cnxnExchange = createExchange[acT.AgentCnxn]()

//       def fresh( seed : acT.AgentCnxn ) : acT.AgentCnxn = {
//      AgentKVDBScope.loadData( numOfEntries )
//      val s = new scala.collection.mutable.HashSet[acT.AgentCnxn]()
//      val cnxnPtn = "cnxn_"
//      val cnxnId = AgentKVDBScope.getUUID
//      val lthrd = cnxnId + ""

//      for(
//        recordsFileName <- importData( lthrd, chunkSize ); 
//        recordsFileNameRoot = recordsFileName.replace( ".xml", "" );
//        recordsFullFileName = currentDir.replace( "/.", "/" + recordsFileName );       
//        clientSession = node.cache.clientSessionFromConfig;
//        cnxn = new acT.AgentCnxn( recordsFileNameRoot.toURI, "", recordsFileNameRoot.toURI );
//        nodePart = node.getLocalPartition( cnxn )
//      ) {
//        val dbName =
//          nodePart.cache.persistenceManifest match {
//            case Some( pd ) => pd.storeUnitStr( cnxn )
//            case None => throw new Exception( "missing persistence manifest" )
//          }
          
//        clientSession.execute( new CreateDB( dbName ) )
//        clientSession.execute( new Add( recordsFullFileName ) )

//        println( "adding cnxn: " + cnxn )       
//        s += cnxn                       
//      }
        
//      s.toList( 0 )
//       }
//       val dummy =
//      new acT.AgentCnxn( "dummy".toURI, "", "dummy".toURI ); 

//       tStream[acT.AgentCnxn]( dummy )( fresh ).drop( 1 )
//     }

//     def setupTestCnxnStream[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
//       node : Being.AgentKVDBNode[ReqBody,RspBody]
//     )(
//       numOfEntries : Int, chunkSize : Int
//     ) : ( Being.AgentKVDBNode[ReqBody,RspBody], Stream[acT.AgentCnxn] ) = {      
//       ( node, cnxnStream( node, numOfEntries, chunkSize ) )
//     }    

    case class TestConfigurationGenerator(
      localHost : String,
      localPort : Int,
      remoteHost : String,
      remotePort : Int,
      dataLocation : String,
      numEntriesSeed : Int,
      chunkSizeSeed : Int,
      numEntriesFloor : Int,
      chunkSizeFloor : Int,
      numNodesSeed : Int,
      numCnxnsSeed : Int,
      nodesFloor : Int,
      cnxnsFloor : Int      
    )

    object StdTestConfigurationGenerator
         extends TestConfigurationGenerator(
           localHost,
           localPort,
           remoteHost,
           remotePort,
           dataLocation,
           numEntriesSeed,
           chunkSizeSeed,
           numEntriesFloor,
           chunkSizeFloor,
           numNodesSeed,
           numCnxnsSeed,
           nodesFloor,
           cnxnsFloor
         )

    case class TestConfiguration[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      @transient
      generator : TestConfigurationGenerator,
      @transient
      pv : PutVal,
      @transient
      vars : Option[List[( String, String )]],
      @transient
      testData : Option[Seq[( Being.AgentKVDBNode[ReqBody,RspBody], Stream[acT.AgentCnxn] )]]
    )    

    object StdTestConfiguration
         extends TestConfiguration[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
           StdTestConfigurationGenerator,
           pvOne,
           Some( List( ( "time", "t" ), ( "host", "host" ) ) ),
           None
         )


    object RemoteTestConfiguration
         extends TestConfiguration[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
           StdTestConfigurationGenerator,
           pvTwo,
           Some( List( ( "time", "t" ), ( "host", "host" ) ) ),
           None
         )

    // def configureTest[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
//       tc : TestConfigurationGenerator
//     ) : Seq[( Being.AgentKVDBNode[ReqBody,RspBody], Stream[acT.AgentCnxn] )] = {
//       import scala.math._
//       def dataPair() : ( Int, Int ) =
//      ( max( ( random * tc.numEntriesSeed ).toInt, tc.numEntriesFloor ), max( ( random * tc.chunkSizeSeed ).toInt, tc.chunkSizeFloor ) )
//       val dataStrm : Stream[( Int, Int )] =
//      tStream[( Int, Int )]( dataPair )( ( seed : ( Int, Int ) ) => dataPair )
//       val agntStrm : Stream[( Being.AgentKVDBNode[ReqBody,RspBody], Stream[acT.AgentCnxn] )] =
//      dataStrm map {
//        ( dp : ( Int, Int ) ) => {
//          val Right( ( client, server ) ) =
//            setup[ReqBody,RspBody](
//              tc.dataLocation, tc.localHost, tc.localPort, tc.remoteHost, tc.remotePort
//            )( true )
//          setupTestCnxnStream[ReqBody,RspBody]( client )( dp._1, dp._2 )
//        }
//      }

//       val testAgentStream = agntStrm.take( max( ( random * tc.numNodesSeed ).toInt, tc.nodesFloor ) )
      
//       for( ( kvdbNode, cnxnStrm ) <- testAgentStream ) yield {
//      ( kvdbNode, cnxnStrm.take( max( ( random * tc.numCnxnsSeed ).toInt, tc.cnxnsFloor ) ) )
//       }
        
//     }
      
//    def  sporeGet[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
//       testConfig : TestConfiguration[ReqBody,RspBody]
//     )( k : Option[mTT.Resource] => Unit ) : Unit = {
//       import scala.math._      

//       for(
//      ( kvdbNode, cnxnStrm ) <- testConfig.testData.getOrElse( configureTest( testConfig.generator ) )        
//       ) {
//      val ptn =
//        testConfig.vars match {
//          case Some( vars ) => {
//            CnxnConversionStringScope.partialCaseClassDerivative( testConfig.pv.asInstanceOf[ScalaObject with Product with Serializable], vars )
//          }
//          case None => {
//            asCnxnCtxtLabel( testConfig.pv.asInstanceOf[ScalaObject with Product with Serializable] )
//          }
//        }
//      for( cnxn <- cnxnStrm ) {
//        reset {
//          for( rsrc <- kvdbNode.get( cnxn )( ptn ) ) {
//            k( rsrc )
//          }
//        }
//      }
//       }
//     }

//     def sporePut[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
//       testConfig : TestConfiguration[ReqBody,RspBody]
//     )( vStream : Stream[String] ) : Unit = {
//       import scala.math._      

//       for( ( kvdbNode, cnxnStrm ) <- testConfig.testData.getOrElse( configureTest( testConfig.generator ) ) ) {
//      val ptn =
//        testConfig.vars match {
//          case Some( vars ) => {
//            CnxnConversionStringScope.partialCaseClassDerivative( testConfig.pv.asInstanceOf[ScalaObject with Product with Serializable], vars )
//          }
//          case None => {
//            asCnxnCtxtLabel( testConfig.pv.asInstanceOf[ScalaObject with Product with Serializable] )
//          }
//        }
//      for( cnxn <- cnxnStrm; v <- vStream ) {
//        reset {
//          kvdbNode.put( cnxn )( ptn, v )
//        }
//      }
//       }
//     }

    def runClient[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      kvdbNode : Being.AgentKVDBNode[ReqBody,RspBody]
    ) : Unit = {
      new Thread {
        override def run() : Unit = {
          reset {
            for( rsrc <- kvdbNode.get( cnxnGlobal )( asCnxnCtxtLabel( "XandY" ) ) ) {
              BasicLogService.tweet( "received: " + rsrc )
            }
          }
        }
      }.start
    }

    def runServer[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      kvdbNode : Being.AgentKVDBNode[ReqBody,RspBody]
    ) : Unit = {
      new Thread {
        override def run() : Unit = {
          reset {
            kvdbNode.put( cnxnGlobal )(
              asCnxnCtxtLabel( "XandY" ), mTT.Ground( "ColdPlay" )
            )
          }
        }
      }.start
    }
        
    def createNode(sourceAddress: URI, acquaintanceAddresses: List[ URI ], configFileName: Option[String]): Being.AgentKVDBNode[ PersistedKVDBNodeRequest, PersistedKVDBNodeResponse ] =
      {
        ptToMany(sourceAddress, acquaintanceAddresses)(configFileName)
      }       
    
 
  }

  object StdAgentUseCase extends AgentUseCase( None )

  object ResubmissionUseCase extends AgentUseCase( None ) {
    import AgentKVDBScope._
    import Being._
    import AgentKVDBNodeFactory._

    import CnxnConversionStringScope._

    import com.protegra_ati.agentservices.store.extensions.StringExtensions._
    @transient
    lazy val restored_privateQ =
      createNode( public_location, List(), None )
    @transient
    val cnxnUIStore =
      new acT.AgentCnxn(
        ( "UI" + UUID.randomUUID.toString ).toURI,
        "",
        ( "Store" + UUID.randomUUID.toString ).toURI
      )
    @transient
    lazy val public_location = "localhost".toURI.withPort( 5672 )
    @transient
    val keyPrivate = "contentRequestPrivate(_)"

    def resubmit() {
      reset {   
        val generator =
          restored_privateQ.resubmitLabelRequests(
            cnxnUIStore
          )( keyPrivate.toLabel )( dAT.AGetNum ).getOrElse( throw new Exception( "No generator!" ) )

        for( placeInstance <- generator ) {
          reset {
            for ( e <- restored_privateQ.get(cnxnUIStore)(keyPrivate.toLabel) ) {
              if ( e != None ) {
                //val result = e.dispatch
                //reset {_resultsQ.put(cnxnTest)(result.toLabel, result+"restored")}
                BasicLogService.tweet( "listen received - " + e )
              }
              else {
                BasicLogService.tweet( "listen received - none" )
              }
            }
          }
        }
      }
    }
  }
}
