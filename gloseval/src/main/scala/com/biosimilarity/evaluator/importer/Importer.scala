// -*- mode: Scala;-*- 
// Filename:    Importer.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jan 19 16:49:16 2016 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.importer

import com.biosimilarity.evaluator.distribution._
import com.biosimilarity.evaluator.Api
import com.biosimilarity.evaluator.Api._
import com.biosimilarity.evaluator.spray.srp.SRPClient
import com.typesafe.config.ConfigFactory

import scalaj.http.HttpOptions
//import com.biosimilarity.evaluator.importer.dtos._
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.spray.NodeUser
import com.biosimilarity.evaluator.spray.util._
import org.json4s.JsonAST.{JObject, JValue}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.json4s.JsonDSL._
import java.util.UUID
import com.biosimilarity.evaluator.spray.srp.ConversionUtils._

import com.biosimilarity.evaluator.omniRPC.OmniClient

import scalaj.http.Http

/*

// maybe later if we end up moving to spray-client instead of scalaj-http

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.Future
import scala.util.{Success, Failure}
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask
import akka.io.IO

import spray.can.Http
import spray.http._
import spray.client.pipelining._

import HttpMethods._
*/



/**
 * Iterates through a sample data file, parses it, and imports the data
 *
 * @note To run the importer, ensure you are using a recent version of Maven.
 * @note First run `mvn clean compile` in the terminal.
 * @note Second run `mvn scala:console`.
 * @note Finally, import this object and execute `Importer.fromFiles()`. You may need to set the file paths in the parameters.
  * @note - it may also be required to reset mongo db with 'mongo records --eval "db.dropDatabase()" '
 */
object Importer extends EvalConfig
  with ImporterConfig
  with Serializable {

  implicit val formats = org.json4s.DefaultFormats
  //implicit val system: ActorSystem = ActorSystem()
  //import system.dispatcher // implicit execution context

  private var GLOSEVAL_HOST = serviceHostURI()
  //private val GLOSEVAL_SENDER = serviceEmailSenderAddress()
  //private val MAILINATOR_HOST = serviceMailinatorHost()
  //private val MAILINATOR_KEY = serviceMailinatorKey()

  private val agentsById = scala.collection.mutable.Map[String, String]()
  // loginId:agentURI
  private val sessionsById = scala.collection.mutable.Map[String, String]()
  // sessionURI:agent
  private val cnxnLabels = scala.collection.mutable.Map[String, String]() // src+trgt:label

  private val labels = scala.collection.mutable.Map[String, LabelDesc]() // id

  private def resolveLabel(id: String): LabelDesc = labels(id)

  private def glosevalPost(data: Api.RequestContent): String = {
    glosevalPost(Api.toReq( data))
  }

  private def glosevalPost(msg : Api.Request): String = {
    val requestBody = write(msg)
    glosevalPost(requestBody)

  }

  private def glosevalPost(msgType: String, data: JValue): String = {
    println(s"REQUEST: $msgType")
    val requestBody = write( ("msgType" -> msgType) ~ ("content" -> data) )
    glosevalPost(requestBody)

  }

  private def glosevalPost(requestBody: String): String = {
    println(s"REQUEST BODY: $requestBody")

    val req = Http(GLOSEVAL_HOST).option(HttpOptions.allowUnsafeSSL)
      .timeout(1000, 60000)
      .header("Content-Type", "application/json")
      .postData(requestBody)
    val response = req.asString.body

    println(s"RESPONSE BODY: $response")
    if (response.startsWith("Malformed request")) throw new Exception(response)
    response
  }



  /*
    private def glosevalPost(requestBody: String): String = {
      println(s"REQUEST BODY: $requestBody")
      //implicit val timeout: Timeout = Timeout(30.seconds)

      val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
      val fut : Future[HttpResponse] = pipeline(Post(GLOSEVAL_HOST, requestBody))

      val rsp : HttpResponse = Await.result(fut,Duration.Inf) //.asInstanceOf[HttpResponse]
      println(s"RESPONSE: ${rsp}")
      //if (rsp.message.startsWith("Malformed request")) throw new Exception(response)
      if (rsp.status.isFailure) {
        throw new Exception("HTTP Error : " + rsp.status + " - " + rsp.status.defaultMessage)
      }
      rsp.entity.asString

  //    Await.ready(fut, Duration.Inf).value.get match {
  //      case Success(rsp) => {
  //        println(s"RESPONSE: ${rsp}")
  //        //if (rsp.message.startsWith("Malformed request")) throw new Exception(response)
  //        if (rsp.status.isFailure) {
  //          throw new Exception("HTTP Error : " + rsp.status + " - " + rsp.status.defaultMessage)
  //        }
  //        rsp.entity.asString
  //      }
  //      case Failure(e) => {
  //        println("Error : " + e)
  //        throw e
  //      }
  //    }
    }
  */


  def makeAliasURI(alias: String) = s"alias://${alias}/alias"

  //private def makeAliasLabel(label: String, color: String) = s"""leaf(text("${label}"),display(color("${color}"),image("")))"""

  /* not used here - just an example of a per session thread approach
  //@@GS - should be actor based but will have to do for now.
  // requires all other methods to be synchronized where appropriate
  def longPollSession(session : String  ) : Thread = {
    println("initiating long-polling for session: " + session)
    new Thread(new Runnable() {
      override def run() {
        while (!Thread.interrupted()) {  // call thrd.interrupt() when session closes
          try {
            val requestBody =  s"{ \"msgType\": \"sessionPing\", \"content\" : {\"sessionURI\": \"${session}\" } }"
            println("Sending Ping ")
            val req = Http(GLOSEVAL_HOST)
                        .timeout(1000,10000)   // connTimeout, readTimeut - wait 10 seconds for response
                                               //  needs to be greater than gloseval clientTimeOut setting - currently 7 seconds
                        .header("Content-Type", "application/json")
                        .postData(requestBody)
            val js = req.asString.body
            println("PING RESPONSE: " + js)
            val arr = parse(js).extract[List[JValue]]
            arr.foreach( v => {
              val typ = ( v \ "msgType").extract[String]
              var cont = ( v \ "content").extract[JValue]
              typ match {
                case "sessionPong" => Thread.sleep(10000)  // loop after 10 seconds
                  // .. dispatch to handlers ...
                case _ => println("WARNING - handler not provided for server sent message type : " + typ)
              }

            })
          } catch {
            case ex : Throwable => {
              println("exception during SessionPing : " + ex)
            }
          }

        }
      }
    })
  }
  */

  var terminateLongPoll = false

  def longPoll(): Thread = {
    println("initiating long-polling")
    new Thread(new Runnable() {
      override def run() {
        while (!terminateLongPoll) {
          val tmp = sessionsById.clone()
          while (tmp.nonEmpty) {
            tmp.clone().foreach {
              case (id, sess) =>
                try {
                  val session = sess
                  val js = glosevalPost(Api.SessionPing(session))
                  val arr = parse(js).extract[List[JObject]]
                  arr.foreach(v => {
                    val typ = (v \ "msgType").extract[String]
                    typ match {
                      case "sessionPong" => tmp.remove(id)
                      case "connectionProfileResponse" => ()
                      case "addAliasLabelsResponse" => ()
                      case "beginIntroductionResponse" => ()
                      case "establishConnectionResponse" => ()
                      case _ =>
                        println("WARNING - handler not provided for server sent message type : " + typ)
                    }
                  })
                } catch {
                  case ex: Throwable => {
                    println("exception during SessionPing : " + ex)
                    tmp.remove(id)
                  }
                }
            }
          }
          if (!terminateLongPoll) {
            println("longpoll sleeping")
            Thread.sleep(10000)
          }
        }
      }
    })
  }

  def expect(msgType: String, session: String): Option[JValue] = {
    println("Sending Ping")
    val js = glosevalPost(Api.SessionPing(session))
    var rslt: Option[JValue] = None
    var done = false
    while (!done) {
      val arr = parse(js).extract[List[JValue]]
      arr.foreach(v => {
        val typ = (v \ "msgType").extract[String]
        typ match {
          case "sessionPong" => done = true
          case `msgType` => {
            done = true
            rslt = Some(v \ "content")
          }
          case _ => ()
        }
      })
    }
    rslt
  }

  def createAgent(agent: AgentDesc): Option[String] = {
    val eml = agent.email + (if (agent.email.contains("@")) "" else "@livelygig.com")
    val jsonBlob = parse(agent.jsonBlob).extract[JObject]
    val srpClient = new SRPClient()
    srpClient.init
    val r1 = parse(glosevalPost(Api.CreateUserStep1Request("noConfirm:" + eml))).extract[ApiResponse]
    r1.responseContent match {
      case ApiError(reason) =>
        println(s"create user, step 1, failed, reason : $reason")
        None
      case CreateUserStep1Response(salt) =>
        srpClient.calculateX(eml, agent.pwd, salt)
        val r2 = parse(glosevalPost(Api.CreateUserStep2Request("noConfirm:" + eml,
          salt, srpClient.generateVerifier, jsonBlob))).extract[ApiResponse]
        r2.responseContent match {
          case ApiError(reason) =>
            println(s"create user, step 2, failed, reason : $reason")
            None
          case CreateUserStep2Response(agentURI) => Some(agentURI)
          case _ => throw new Exception("Unspecified response")
        }
      case _ => throw new Exception("Unspecified response")
    }
  }

  def createSession(agentURI: String, email: String, pwd: String): Option[String] = {
    val srpClient = new SRPClient()
    srpClient.init
    val r1 = parse(glosevalPost(Api.InitializeSessionStep1Request(s"$agentURI?A=${srpClient.calculateAHex}")))
      .extract[ApiResponse]
    r1.responseContent match {
      case ApiError(reason) =>
        println(s"initialize session, step 1, failed, reason : $reason")
        None
      case InitializeSessionStep1Response(salt, bval) =>
        srpClient.calculateX(email, pwd, salt)
        val r2 = parse(glosevalPost(Api.InitializeSessionStep2Request(s"$agentURI?M=${srpClient.calculateMHex(bval)}")))
          .extract[ApiResponse]
        r2.responseContent match {
          case ApiError(reason) =>
            println(s"initialize session, step 2, failed, reason : $reason")
            None
          case InitializeSessionResponse(sessionURI, m2) =>
            if(srpClient.verifyServerEvidenceMessage(fromHex(m2))) Some(sessionURI)
            else throw new Exception("Authentication failed on client")
          case _ => throw new Exception("Unspecified response")
        }
      case _ => throw new Exception("Unspecified response")
    }
  }

  def makeAgent(agent: AgentDesc): Unit = {
    createAgent(agent) match {
      case None => ()
      case Some(agentURI) =>
        val agentCap = agentURI.replace("agent://cap/", "").slice(0, 36)
        agentsById.put(agent.id, agentCap)
        createSession(agentURI, agent.email, agent.pwd) match {
          case None => throw new Exception("Create session failure.")
          case Some(session) =>
            sessionsById.put(agent.id, session)

            agent.aliasLabels match {
              case None =>()
              case Some(l) =>
                val lbls = l.map(lbl => makeLabel(LabelDesc.extractFrom(lbl)).toTermString(resolveLabel))
                glosevalPost(Api.AddAliasLabelsRequest(session, "alias", lbls))
            }
        }
      case _ => throw new Exception("Unspecified response")
    }
  }

  def makeLabel(label: LabelDesc): LabelDesc = {

    def matchFunctor(name: String, lbl: LabelDesc): Boolean = {
      lbl match {
        case ComplexLabelDesc(_, fnctr, _) => name == fnctr
        case SimpleLabelDesc(_, _, Some(fnctr)) => name == fnctr
        case _ => false
      }
    }

    def reorderComponents(lbl: LabelDesc): LabelDesc = {
      lbl match {
        case ComplexLabelDesc(id, "leaf", lbls) => {
          val (tp, r) = lbls.partition(matchFunctor("text", _))
          if (tp.length > 1) throw new Exception("label must contain at most one text field")
          val (dp, r2) = r.partition(matchFunctor("display", _))
          if (dp.length > 1) throw new Exception("label must contain at most one display field")
          val lbls2 = tp ++ dp ++ r2
          ComplexLabelDesc(id, "leaf", lbls2)
        }
        case _ => lbl
      }
    }

    label match {
      case smpl: SimpleLabelDesc => {
        smpl.id.foreach(s => labels.put(s, smpl))
        smpl
      }
      case cmplx: ComplexLabelDesc => {
        val rslt = reorderComponents(cmplx)
        cmplx.id.foreach(s => labels.put(s, rslt))
        rslt
      }
      case ref: LabelRef => ref //labels(ref.label)  // throw if not present??
    }

  }

  def makeCnxn(sessionId: String, connection: ConnectionDesc): Unit = {
    try {
      val sourceId = agentsById(connection.src.replace("agent://", ""))
      val sourceURI = makeAliasURI(sourceId)
      val targetId = agentsById(connection.trgt.replace("agent://", ""))
      val targetURI = makeAliasURI(targetId)
      val cnxnLabel = UUID.randomUUID().toString

      if (!cnxnLabels.contains(sourceId + targetId)) {
        glosevalPost(Api.EstablishConnectionRequest(sessionId, sourceURI, targetURI, cnxnLabel))
        cnxnLabels.put(sourceId + targetId, cnxnLabel)
        cnxnLabels.put(targetId + sourceId, cnxnLabel)
      }
    } catch {
      case ex: Throwable => println("exception while creating connection: " + ex)
    }
  }

  def makePost(post: PostDesc): Unit = {
    try {
      var cnxns: List[Api.Connection] = Nil

      val sourceId = agentsById(post.src)
      val sourceAlias = makeAliasURI(sourceId)
      val sourceSession = sessionsById(post.src)

      val selfcnxn = Api.Connection("agent://" + sourceId, "agent://" + sourceId, "alias")
      //val selfcnxn = Connection(sourceAlias, sourceAlias, "alias")

      post.trgts.foreach(trgt => {
        val targetId = agentsById(trgt)
        val lbl = cnxnLabels(sourceId + targetId)
        val trgtAlias = makeAliasURI(agentsById(trgt))
        cnxns = Api.Connection(sourceAlias, trgtAlias, lbl) :: cnxns
      })

      val cont = Api.EvalSubscribeContent(selfcnxn :: cnxns, post.label, post.value, post.uid)
      glosevalPost(Api.EvalSubscribeRequest(sourceSession, Api.EvalSubscribeExpression("insertContent", cont)))

    } catch {
      case ex: Throwable => println("exception while creating post: " + ex)
    }
  }

  def parseData(dataJsonFile: String = serviceDemoDataFile()) = {
    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    parse(dataJson).extract[DataSetDesc]
  }

  def getAgentURI(email: String, password: String) = {
    val json = glosevalPost(Api.GetAgentRequest(email, password))
    val jsv = parse(json)

    val tmsg = (jsv \ "msgType").extract[String]
    if (tmsg == "getAgentError") {
      println("create user failed, reason : " + (jsv \ "content" \ "reason").extract[String])
      None
    }
    else {
      val agentURI = (jsv \ "content" \ "agentURI").extract[String]
      Some(agentURI)
    }
  }

  def fromFile(dataJsonFile: String, host: String = GLOSEVAL_HOST): Unit = {

    println("launching server")
    //resetMongo()
    //Thread.sleep(5000)  // give it time to warm up ...

    println("Importing file : " + dataJsonFile)
    GLOSEVAL_HOST = host

    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val dataset = parse(dataJson).extract[DataSetDesc]

    getAgentURI(NodeUser.email, NodeUser.password) match {
      case Some(uri) => {
        val adminId = uri.replace("agent://", "")
        try {
          val adminSession = createSession(uri, NodeUser.email, NodeUser.password).get
          sessionsById.put(adminId, adminSession) // longpoll on adminSession
          println("using admin session URI : " + adminSession)
          val thrd = longPoll()
          thrd.start()
          val isok = glosevalPost(Api.ResetDatabaseRequest(adminSession))
          if (isok != "OK") throw new Exception("Unable to reset database")

          dataset.labels match {
            case Some(lbls) => lbls.foreach(l => makeLabel(LabelDesc.extractFrom(l)))
            case None => ()
          }

          dataset.agents.foreach(makeAgent)

          dataset.cnxns match {
            case Some(cnxns) => cnxns.foreach(cnxn => makeCnxn(adminSession, cnxn))
            case None => ()
          }

          dataset.posts match {
            case Some(posts) => posts.foreach(makePost)
            case None => ()
          }
        } finally {
          terminateLongPoll = true
        }
      }
      case _ => throw new Exception("Unable to open admin session")
    }

  }

  def runTestFile(dataJsonFile: String): Unit = {
    // this routine doesn't keep sessions alive via longpoll.
    // the calls to expect will
    println("testing file : " + dataJsonFile)
    val host = GLOSEVAL_HOST

    val adminURI =
      getAgentURI(NodeUser.email, NodeUser.password) match {
        case Some(uri) => uri
        case _ => throw new Exception("unable to open admin session")
      }
    val adminId = adminURI.replace("agent://", "")
    val adminSession = createSession(adminURI, NodeUser.email, NodeUser.password).get
    sessionsById.put(adminId, adminSession) // longpoll on adminSession
    println("using admin session URI : " + adminSession)
    var testOmni = EvalConfConfig.isOmniRequired()

    def runTests(ssn : String, tests : List[JObject]) : Unit = {
      tests.foreach(el => {
        val typ = (el \ "type").extract[String]
        typ match {
          case "spawn" =>
            val js = glosevalPost(Api.SpawnSessionRequest(ssn))
            val tssn = (parse(js) \ "content" \ "sessionURI").extract[String]
            println(tssn)
            val tsts = (el \ "content").extract[List[JObject]]
            runTests(tssn, tsts)
            glosevalPost(Api.CloseSessionRequest(tssn))

          case "reset" => resetMongo()
//            val isok = glosevalPost(Api.ResetDatabaseRequest(ssn))
//            if (isok != "OK") throw new Exception("Unable to reset database")
//            expect("resetDatabaseResponse", ssn) match {
//              case Some(_) => ()
//              case _ => throw new Exception("Unable to reset database")
//            }

          case "startCam" =>
            val js = glosevalPost(Api.StartSessionRecording(ssn))
            println(js)

          case "stopCam" =>
            glosevalPost(Api.SessionPing(ssn))  // make sure messages get returned to us before closing the cam
            val js = glosevalPost(Api.StopSessionRecording(ssn))
            //println(js)
            val els = parse(js).extract[List[JObject]]
            els foreach (el => println(el))

          case "agent" =>
            val agent = (el \ "content").extract[AgentDesc]
            makeAgent(agent)
            if (testOmni) {
              val session = sessionsById(agent.id)
              val isok = glosevalPost(Api.GetAmpWalletAddress(session))
              if (isok != "OK") throw new Exception("Unable to call getAmpWalletAddress")
              expect("getAmpWalletAddressResponse", session) match {
                case Some(js) => {
                  //println(pretty(js))
                  val oldaddr = (js \ "address").extract[String]
                  val isok2 = glosevalPost(Api.SetAmpWalletAddress(session, OmniClient.testAmpAddress))
                  if (isok2 != "OK") throw new Exception("Unable to call setAmpWalletAddress")
                  expect("setAmpWalletAddressResponse", session) match {
                    case Some(js) => {
                      //println(pretty(js))
                      val newaddr = (js \ "newaddress").extract[String]
                      if (newaddr != OmniClient.testAmpAddress) throw new Exception("setAmpWalletAddressResponse invalid")
                      val isok3 = glosevalPost(Api.SetAmpWalletAddress(session, oldaddr))
                      if (isok3 != "OK") throw new Exception("Unable to call setAmpWalletAddress")
                      expect("setAmpWalletAddressResponse", session) match {
                        case Some(js) => {
                          val addr = (js \ "newaddress").extract[String]
                          if (addr != oldaddr) throw new Exception("setAmpWalletAddressResponse invalid")
                        }
                        case _ => throw new Exception("Unable to set wallet address")
                      }
                    }
                    case _ => throw new Exception("Unable to set wallet address")
                  }
                }
                case _ => throw new Exception("Unable to get wallet address")
              }
              testOmni = false // once is enough ...
            }

          case "cnxn" =>
            val cnxn = (el \ "content").extract[ConnectionDesc]
            makeCnxn(ssn, cnxn)

          case "label" =>
            val jo = (el \ "content").extract[JObject]
            val lbl = LabelDesc.extractFrom(jo)
            makeLabel(lbl)

          case "post" =>
            val post = (el \ "content").extract[PostDesc]
            makePost(post)

          case _ => throw new Exception("Unknown test element")
        }
      })
    }

    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val tests = parse(dataJson).extract[List[JObject]]
    runTests(adminSession, tests)

    if (testOmni) OmniClient.runTests()
  }

  def fromFiles(dataJsonFile: String = serviceDemoDataFile(), host: String = GLOSEVAL_HOST): Unit = {
    fromFile(dataJsonFile, host)
    //fromFile("src/main/resources/sample-data-test_2.json", host)
  }

  def runTestFiles(): Unit = {
    runTestFile(ConfigFactory.load.getString("importer.test.file"))
  }
}
