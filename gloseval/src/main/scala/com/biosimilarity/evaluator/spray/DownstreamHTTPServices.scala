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

case class MyObj(str:String, i:Int)

object DownStreamHttpComms extends EvaluationCommsService
     with EvalConfig
     with DSLCommLinkConfiguration
     with AccordionConfiguration
     with Serializable
{
  import DSLCommLink.mTT
  import ConcreteHL._
  //implicit val myObjFormat = jsonFormat2(MyObj)
  
  def ask(
    rspCnxn : PortableAgentCnxn,
    rspLabel : CnxnCtxtLabel[String,String,String],
    rqURL : URL,
    onPost : Option[mTT.Resource] => Unit = {
      ( optRsrc : Option[mTT.Resource] ) => BasicLogService.tweet( "post response: " + optRsrc )
    }
  ) {
    import concurrent.ExecutionContext.Implicits._

    //val obj = MyObj("hello", 1)   
    //val req = Post("/some/url", obj) ~> addHeader("X-Foo", "bar")
    val req = Get( rqURL.getPath ) 
    val host = rqURL.getHost
    val port = rqURL.getPort

    implicit val system = ActorSystem()
    val ioBridge = IOExtension(system).ioBridge()
    val httpClient = system.actorOf(Props(new HttpClient(ioBridge)))

    val conduit = system.actorOf(
      //props = Props(new HttpConduit(httpClient, "localhost", 8080)),
      //props = Props(new HttpConduit(httpClient, "ip.jsontest.com", 80)),
      props = Props(new HttpConduit(httpClient, host, port)),
      name = "http-conduit"
    )

    val pipeline = HttpConduit.sendReceive(conduit)
    val response: Future[HttpResponse] = pipeline(req)
    response onComplete{
      case Failure(ex) => ex.printStackTrace()
      case Success(resp) => {
        post( rspLabel, List( rspCnxn ), resp, onPost )
      }
    }
  }
}
