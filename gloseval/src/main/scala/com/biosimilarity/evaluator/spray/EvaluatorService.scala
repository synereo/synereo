package com.biosimilarity.evaluator.spray

import scala.concurrent.duration._
import akka.actor._
import akka.actor.Actor
import spray.util._
import spray.routing._
import spray.http._
import MediaTypes._
import HttpMethods._


// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class EvaluatorServiceActorOne extends Actor with EvaluatorService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait EvaluatorService extends HttpService {

  val myRoute =
    path("") {
      get {
        respondWithMediaType(`text/html`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            <html>
              <body>
                <h1>Say hello to <i>spray-routing</i> on <i>Jetty</i>!</h1>
              </body>
            </html>
          }
        }
      }
    }

}

class EvaluatorServiceActor extends Actor with SprayActorLogging {
  //self : Actor with SprayActorLogging =>

  def receive = {
    case HttpRequest(GET, "/", _, _, _) =>
      sender ! index

    case HttpRequest(GET, "/ping", _, _, _) =>
      sender ! HttpResponse(entity = "PONG!")

    case HttpRequest(GET, "/stream", _, _, _) =>
      val peer = sender // since the Props creator is executed asyncly we need to save the sender ref
      context.actorOf(Props(new Streamer(peer, 20)))

    case HttpRequest(GET, "/crash", _, _, _) =>
      sender ! HttpResponse(entity = "About to throw an exception in the request handling actor, " +
        "which triggers an actor restart")
      throw new RuntimeException("BOOM!")

    case HttpRequest(GET, "/timeout", _, _, _) =>
      log.info("Dropping request, triggering a timeout")

    case HttpRequest(GET, "/timeout/timeout", _, _, _) =>
      log.info("Dropping request, triggering a timeout")

    case HttpRequest(POST, uri, hdrs, entity, _) => {
      println( "uri: " + uri );
      println( "hdrs: " + hdrs );      
      entity.foreach(
	( cntntType : ContentType, bytes : Array[Byte] ) => {
	  println( "bytes: " + bytes )
	}
      )
      sender ! stockMsgResponse
    }

    case _: HttpRequest => sender ! HttpResponse(404, "Unknown resource!")

    // case Timedout(HttpRequest(_, "/timeout/timeout", _, _, _)) =>
//       log.info("Dropping Timeout message")

//     case Timedout(request: HttpRequest) =>
//       sender ! HttpResponse(500, "The " + request.method + " request to '" + request.uri + "' has timed out...")
  }

  ////////////// helpers //////////////

  lazy val index = HttpResponse(
    entity = HttpBody(`text/html`,
      <html>
        <body>
          <h1>Say hello to <i>spray-servlet</i>!</h1>
          <p>Defined resources:</p>
          <ul>
            <li><a href="/ping">/ping</a></li>
            <li><a href="/stream">/stream</a></li>
            <li><a href="/crash">/crash</a></li>
            <li><a href="/timeout">/timeout</a></li>
            <li><a href="/timeout/timeout">/timeout/timeout</a></li>
          </ul>
        </body>
      </html>.toString
    )
  )

  lazy val stockMsgResponse = HttpResponse(
    entity = HttpBody(`application/json`,
      "{ \"msgType\" : \"evalComplete\", \"contents\" : { \"sessionURI\" : \"agent-session://myLovelySession/1234, \" \"pageOfPosts\" : [] }"
    )
  )

  // simple case class whose instances we use as send confirmation message for streaming chunks
  case class Ok(remaining: Int)

  class Streamer(peer: ActorRef, count: Int) extends Actor with SprayActorLogging {
    log.debug("Starting streaming response ...")

    // we use the successful sending of a chunk as trigger for scheduling the next chunk
    peer ! ChunkedResponseStart(HttpResponse(entity = " " * 2048)).withSentAck(Ok(count))

    def receive = {
      case Ok(0) =>
        log.info("Finalizing response stream ...")
        peer ! MessageChunk("\nStopped...")
        peer ! ChunkedMessageEnd()
        context.stop(self)

      case Ok(remaining) =>
        log.info("Sending response chunk ...")
        context.system.scheduler.scheduleOnce(100 millis span) {
          peer ! MessageChunk(DateTime.now.toIsoDateTimeString + ", ").withSentAck(Ok(remaining - 1))
        }

      case x: IOClosed =>
        log.info("Canceling response stream due to {} ...", x.reason)
        context.stop(self)
    }
  }

}


