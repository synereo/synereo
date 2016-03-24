// -*- mode: Scala;-*- 
// Filename:    DownstreamHTTPServices.scala 
// Authors:     lgm                                                    
// Creation:    Wed Apr  2 12:21:34 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.model.store._

//import spray.httpx.RequestBuilding._
import akka.actor.ActorSystem
import spray.http._
//import spray.io.IOExtension

//@@GS
//import spray.can.client.HttpClient
//import spray.client.HttpConduit
import akka.io.IO
import spray.can.Http
import spray.client.pipelining._

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait DownStreamHttpCommsT
{
  self : EvaluationCommsService => 
  import BlockChainAPI._
  import DSLCommLink.mTT
  
  def ask[Data <: BlockChainData](
    rspCnxn : PortableAgentCnxn,
    rspLabel : CnxnCtxtLabel[String,String,String],
    rq : BlockChainCall[Data],
    onPost : Option[mTT.Resource] => Unit = {
      ( optRsrc : Option[mTT.Resource] ) => BasicLogService.tweet( "post response: " + optRsrc )
    }
  ) {
    //import concurrent.ExecutionContext.Implicits._
    //import scala.concurrent.ExecutionContext.Implicits._

    //val req = Post( rq.url.getPath, FormData( toMap( rq.data ) ) )
    /*
    val host = rq.url.getHost
    val port = 
      if ( rq.url.getPort < 0 ) { 80 } else { rq.url.getPort }
    */
    //@@GS - not defaulting the port to 80 when < 0 ...
    val req = Post( rq.url.toString , FormData( toMap( rq.data ) ) )

    implicit val system = ActorSystem()
    //@@GS
    //val ioBridge = IOExtension(system).ioBridge()
    //val httpClient = system.actorOf(Props(new HttpClient(ioBridge)))

    //val conduit = system.actorOf(
    //  props = Props(new HttpConduit(httpClient, host, port)),
    //  name = "http-conduit"
    //)

    //val pipeline = HttpConduit.sendReceive(conduit)

    import system.dispatcher  // execution context - @@GS ???

    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
/*
    val pipeline : Future[SendReceive] =
      for (
        Http.HostConnectorInfo(connector, _) <-
            IO(Http) ? Http.HostConnectorSetup(host, port )
      ) yield sendReceive(connector)
*/

    val response: Future[HttpResponse] = pipeline(req) //.flatMap(_(req))

    response onComplete{
      case Failure(ex) => ex.printStackTrace()
      case Success(resp) => {
        val blockChainData = resp.entity.asString
        //println( "HTTP response to " + rq.url + rq.data + " is " + resp.toString )
        println( "Blockchain data" +  " is " + blockChainData )
        println(
          (
            "*********************************************************************************"
            + "\nputting Blockchain response data "
            + "\nrspCnxn: " + rspCnxn
            + "\nrpsLabel: " + rspLabel
            + "\nblockChainData: " + blockChainData
            + "\n*********************************************************************************"
          )
        )
        put( rspLabel, List( rspCnxn ), blockChainData, onPost )
      }
    }
  }
}
