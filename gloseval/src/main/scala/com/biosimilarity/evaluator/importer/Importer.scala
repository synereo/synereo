// -*- mode: Scala;-*- 
// Filename:    Importer.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jan 19 16:49:16 2016 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.importer

import java.util.UUID

import com.biosimilarity.evaluator.distribution.{ AccordionConfiguration, DSLCommLinkConfiguration, EvalConfig, EvaluationCommsService }
import com.biosimilarity.evaluator.importer.dtos._
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.importer.utils.mailinator.Mailinator
import com.biosimilarity.evaluator.spray.{ BTCHandler, DownStreamHttpCommsT, EvalHandler }
import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.joda.time.DateTime
import scala.collection.JavaConversions._
import scala.util.Random
import scala.collection.mutable.HashMap

import scalaj.http.{ HttpOptions, Http }

/**
 * Iterates through a sample data file, parses it, and imports the data
 *
 * @note To run the importer, ensure you are using a recent version of Maven.
 * @note First run `mvn clean compile` in the terminal.
 * @note Second run `mvn scala:console`.
 * @note Finally, import this object and execute `Importer.fromFiles()`. You may need to set the file paths in the parameters.
 */
object Importer extends EvalConfig
  with ImporterConfig
  with Serializable {

  import org.json4s.jackson.Serialization

  implicit val formats = org.json4s.DefaultFormats

  private var GLOSEVAL_HOST = serviceHostURI()
  private val GLOSEVAL_SENDER = serviceEmailSenderAddress()
  private val MAILINATOR_KEY = serviceMailinatorKey()

  private val agentsById = scala.collection.mutable.Map[String, String]() // loginId:agentURI
  private val agentsBySession = scala.collection.mutable.Map[String, AgentDesc]() // sessionURI:agent
  private val sessionsById = scala.collection.mutable.Map[String, InitializeSessionResponse]() // sessionURI:agent
  private val aliasesById = scala.collection.mutable.Map[String, String]() // agentId:aliasUri

  private def glosevalPost(msgType: String, data: RequestContent): String = {
    val requestBody = write(ApiRequest(msgType, data))
    println(s"REQUEST: ${msgType}")
    println(s"REQUEST BODY: ${requestBody}")

    val req = Http(GLOSEVAL_HOST)
      .timeout(1000, 15000)
      .header("Content-Type", "application/json")
      .postData(requestBody)
    val response = req.asString.body

    println(s"RESPONSE BODY: ${response}")
    response
  }

  //private def makeAliasLabel(label: String, color: String) = s""" "leaf(text("${label}"),display(color("${color}"),image("")))" """.trim
  private def makeAliasLabel(label: String, color: String) = "leaf(text(\"" + label + "\"),display(color(\"" + color + "\"),image(\"\")))"

  private def threadSleep(seconds: Int) = {
    println(s"not Sleeping for $seconds seconds")
    //Thread.sleep(seconds.toLong * 1000)
    /*
    def time = new DateTime().getMillis
    var now = time
    val future = now + (seconds.toLong * 1000)
    while (time < future) {
      now = time
    }
    */
  }

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

  //@@GS - should be actor based but will have to do for now.
  // requires all other methods to be synchronized where appropriate
  def longPoll(): Thread = {
    println("initiating long-polling")
    new Thread(new Runnable() {
      override def run() {
        while (!Thread.interrupted()) {
          val tmp = agentsBySession.clone()
          while (tmp.nonEmpty) {
            tmp.foreach {
              case (session, _) =>
                try {
                  println("Sending Ping")
                  val js = glosevalPost("sessionPing", SessionPingRequest(session))
                  val arr = parse(js).extract[List[JValue]]
                  arr.foreach(v => {
                    val typ = (v \ "msgType").extract[String]
                    typ match {
                      case "sessionPong" => tmp.remove(session)
                      case _ => {
                        println("WARNING - handler not provided for server sent message type : " + typ)
                        //println("contents : " + (v \ "content"))
                      }
                    }

                  })
                } catch {
                  case ex: Throwable => {
                    println("exception during SessionPing : " + ex)
                  }
                }
            }
          }
          Thread.sleep(10000)
        }
      }
    })
  }

  def makeAgent(agent: AgentDesc): Unit = {
    val blobMap = new HashMap[String, String]()
    val agentName = agent.firstName + " " + agent.lastName
    blobMap += ("name" -> agentName)

    val jstmp = glosevalPost(
      "createUserRequest",
      CreateUserRequest(
        "",
        agent.pwd,
        blobMap,
        true))

    //threadSleep(30)

    val agentURI = (parse(jstmp) \ "content" \ "agentURI").extract[String]
    val agentId = agentURI.replace("agent://", "")
    agentsById.put(agent.id, agentId)

    val json =
      glosevalPost(
        "initializeSessionRequest",
        InitializeSessionRequest(agentURI + "?password=" + agent.pwd))

    val jsession: JValue = (parse(json) \ "content")
    val uri = (jsession \ "sessionURI").extract[String]
    val alias = (jsession \ "defaultAlias").extract[String]

    val session = InitializeSessionResponse(uri, alias, jsession)

    agentsBySession.put(session.sessionURI, agent)
    sessionsById.put(agent.id, session)

    glosevalPost("addAliasLabelsRequest", AddAliasLabelsRequest(session.sessionURI, "alias", List(makeAliasLabel(agentId, "#5C9BCC"))))
    aliasesById.put(agent.id, s"alias://${agentId}/alias")

  }

  def makeCnxns(cnxns : Seq[ConnectionDesc]): Unit = synchronized {
    println("Creating connection introductions")
    cnxns.foreach { connection =>
      try {
        val sourceId = connection.src.replace("agent://","")
        val sourceAlias = aliasesById(sourceId)
        val sourceURI = sessionsById(sourceId).sessionURI
        val targetId = connection.trgt.replace("agent://","")
        val targetAlias = aliasesById(targetId)
        //val alias = sourceAlias + "-" + targetAlias
        val lbl: String = connection.label match {
          case Some(LabelDesc(_, value, _)) => value
          case None => UUID.randomUUID.toString()
        }
        val aMessage = ""
        val bMessage = ""
        glosevalPost("beginIntroductionRequest", BeginIntroductionRequest(sourceURI, "alias", Connection(sourceAlias, targetAlias, lbl), Connection(targetAlias, sourceAlias, lbl), aMessage, bMessage))
      } catch {
        case ex: Throwable => println("exception while creating connection: " + ex)
      }
    }
    println("Random introduction connections complete")
  }

  def parseData(
    dataJsonFile: String = serviceDemoDataFile(),
    configJsonFile: String = serviceSystemLabelsFile()) = {
    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    parse(dataJson).extract[DataSetDesc]
  }

  def fromFiles(
    dataJsonFile: String = serviceDemoDataFile(),
    configJsonFile: String = serviceSystemLabelsFile(),
    host: String = GLOSEVAL_HOST,
    chunkSize: Int = 4) {
    println("Beginning import procedure")
    GLOSEVAL_HOST = host

    //val configJson = scala.io.Source.fromFile(configJsonFile).getLines.map(_.trim).mkString
    //val config = parse(configJson).extract[ConfigDesc]

    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val dataset = parse(dataJson).extract[DataSetDesc]

    val thrd = longPoll()
    thrd.start()

    dataset.agents.foreach(makeAgent)

    makeCnxns(dataset.cnxns)

    // need to fix this
    // wait ten seconds for long poll receipts
    thrd.interrupt()

    thrd.join(10000)

    // iterate and create connection confirmations
    /*dataset.cnxns.foreach { connection =>
      val sessionId = connection.src+":::"+connection.trgt
      
      val jConnection: JValue =
        ("content" ->
          ("sessionURI" -> sessionId) ~
          ("alias" -> "someURI") ~ // TODO: this is actually the agent URI that comes back from API
          ("introSessionId" -> "someURI") ~ // TODO: this is the session URI
          ("correlationId" -> "someURI") ~ // TODO: correlationId comes back when calling `beginIntroductionRequest`
          ("accepted" -> true)
        )
      introductionConfirmationRequest(jConnection)

      val jConnection: JValue =
        ("content" ->
          ("sessionURI" -> sessionId) ~
            ("alias" -> "someURI") ~ // TODO: this is actually the agent URI that comes back from API
            ("introSessionId" -> "someURI") ~ // TODO: this is the session URI
            ("correlationId" -> "someURI") ~ // TODO: correlationId comes back when calling `beginIntroductionRequest`
            ("accepted" -> true)
          )
      introductionConfirmationRequest(jConnection)
    }*/

  }

}
