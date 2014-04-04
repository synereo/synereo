// -*- mode: Scala;-*- 
// Filename:    DownstreamHTTPServices.scala 
// Authors:     lgm                                                    
// Creation:    Wed Apr  2 12:21:34 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

import com.protegra_ati.agentservices.store._

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.msgs._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import spray.httpx.RequestBuilding._
import spray.http._
import HttpMethods._
import HttpHeaders._
import MediaTypes._
import spray.json._
import DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport._
import akka.actor.ActorSystem
import spray.io.IOExtension
import akka.actor.Props
import spray.can.client.HttpClient
import spray.client.HttpConduit
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import java.net.URL

trait DownStreamHttpCommsT
{
  self : EvaluationCommsService => 
  import DSLCommLink.mTT
  import ConcreteHL._  
  import BlockChainAPI._
  
  def ask[Data <: BlockChainData](
    rspCnxn : PortableAgentCnxn,
    rspLabel : CnxnCtxtLabel[String,String,String],
    rq : BlockChainCall[Data],
    onPost : Option[mTT.Resource] => Unit = {
      ( optRsrc : Option[mTT.Resource] ) => BasicLogService.tweet( "post response: " + optRsrc )
    }
  ) {
    import concurrent.ExecutionContext.Implicits._

    val req = Post( rq.url.getPath, FormData( toMap( rq.data ) ) ) 
    val host = rq.url.getHost
    val port = 
      if ( rq.url.getPort < 0 ) { 80 } else { rq.url.getPort }

    implicit val system = ActorSystem()
    val ioBridge = IOExtension(system).ioBridge()
    val httpClient = system.actorOf(Props(new HttpClient(ioBridge)))

    val conduit = system.actorOf(
      props = Props(new HttpConduit(httpClient, host, port)),
      name = "http-conduit"
    )

    val pipeline = HttpConduit.sendReceive(conduit)
    val response: Future[HttpResponse] = pipeline(req)

    response onComplete{
      case Failure(ex) => ex.printStackTrace()
      case Success(resp) => {
        val blockChainData = resp.entity.asString
        //println( "HTTP response to " + rq.url + rq.data + " is " + resp.toString )
        println( "Blockchain data" +  " is " + blockChainData )
        println(
          (
            "*********************************************************************************"
            + "\nputting Blockchain wallet data "
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
