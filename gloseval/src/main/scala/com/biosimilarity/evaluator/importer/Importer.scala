// -*- mode: Scala;-*- 
// Filename:    Importer.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jan 19 16:49:16 2016 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.importer

import com.biosimilarity.evaluator.distribution.{AccordionConfiguration, DSLCommLinkConfiguration, EvalConfig, EvaluationCommsService}
import com.biosimilarity.evaluator.importer.dtos._
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.importer.utils.mailinator.Mailinator
import com.biosimilarity.evaluator.spray.{BTCHandler, DownStreamHttpCommsT, EvalHandler}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.joda.time.DateTime
import scala.collection.JavaConversions._
import scala.util.Random
import scala.collection.mutable.HashMap

import scalaj.http.Http

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

  //private var GLOSEVAL_HOST = "http://52.35.39.85:9876/api"
  private var GLOSEVAL_HOST = serviceHostURI()
  //private val GLOSEVAL_SENDER = "splicious.ftw@gmail.com"
  private val GLOSEVAL_SENDER = serviceEmailSenderAddress()
  //private val MAILINATOR_KEY = "efa3a1b773db4f0c9492686d24bed415"
  private val MAILINATOR_KEY = serviceMailinatorKey()

  private def glosevalPost(msgType: String, data: RequestContent): String = {
    val requestBody = write(ApiRequest(msgType, data))
    println(s"REQUEST: ${msgType}")
    println(s"REQUEST BODY: ${requestBody}")

    val response = Http(GLOSEVAL_HOST).postData(requestBody).header("Content-Type", "application/json").asString.body

    println(s"RESPONSE BODY: ${response}")
    response
  }

  private def createEmailUser(loginId: String) = s"livelygig-$loginId"

  private def createEmailAddress(loginId: String) = s"${createEmailUser(loginId)}@mailinator.com"

  private def makeAliasURI(alias: String) = s"alias://$alias/alias"

  private def makeAliasLabel(label: String, color: String) = s""" "leaf(text("${label}"),display(color("${color}"),image("")))" """.trim

  private def threadSleep(seconds: Int) = {
    println(s"Sleeping for $seconds seconds")
    def time = new DateTime().getMillis
    var now = time
    val future = now + (seconds.toLong*1000)
    while(time < future) {
      now = time
    }
  }

  private val sessionsByAgent = scala.collection.mutable.Map[String, InitializeSessionResponse]() // loginId:agentURI
  private val agentsBySession = scala.collection.mutable.Map[String, AgentDesc]() // sessionURI:agent
  private val sessionsById = scala.collection.mutable.Map[String, InitializeSessionResponse]() // sessionURI:agent
  private val aliasesById = scala.collection.mutable.Map[String, String]() // agentId:aliasUri

  def makeAgent( agent : AgentDesc ) : Unit = {
    val blobMap = new HashMap[String,String]()
    val agentName = s"{agent.firstName}" + " " + s"{agent.lastName}"
    blobMap += ( "name" -> agentName )
    val json =
      // glosevalPost(
//         "initializeSessionRequest",
//         InitializeSessionRequest(
//           s"agent://email/${createEmailAddress(agent.loginId)}?password=${agent.pwd}"
//         )
//       )
      glosevalPost(
        "createUserRequest",
        // InitializeSessionRequest(
//           s"agent://email/${createEmailAddress(agent.loginId)}?password=${agent.pwd}"
//         )
        CreateUserRequest(
          s"{createEmailAddress(agent.loginId)}",
          s"{agent.pwd}",
          blobMap,
          true
        )
      )

    val session = parse(json).extract[ApiResponse[InitializeSessionResponse]].content
    
    sessionsByAgent.put(agent.loginId, session)
    agentsBySession.put(session.sessionURI, agent)
    sessionsById.put(agent.id, session)
  }

  // paginated agent creation
  def makeAgents( dataset : DataSetDesc, chunkSize : Int = 4 ) : Unit = {
    println("Initializing agent sessions")
    val agents = dataset.agents

    val numOfAgents = agents.length
    val numOfChunks = numOfAgents / chunkSize    
    val leftOvers = ( numOfAgents % chunkSize )

    for( i <- ( 1 to numOfChunks ) ) {
      for( j <- ( 1 to chunkSize ) ) {
        makeAgent( agents( ( i - 1 ) * chunkSize + ( j - 1 ) ) )        
        threadSleep(15)
      }      
      
      threadSleep(30)
    }
    if ( leftOvers > 0 ) {
      for ( j <- ( 1 to leftOvers ) ) {
        makeAgent( agents( numOfChunks * chunkSize + ( j - 1 ) ) )        
        threadSleep(15)
      }
      threadSleep(30)
    }

    println("Agent session initialization complete")
        
  }

  def makeLabels( agentSessions : scala.collection.mutable.Map[String, AgentDesc] ) : Unit = {
    println("Adding labels for agents")
    agentsBySession.foreach { case (session, agent) =>
      val json = glosevalPost("addAliasLabelsRequest", AddAliasLabelsRequest(session, "alias", List(makeAliasLabel(agent.id, "#5C9BCC"))))
      println(json)
      aliasesById.put(agent.id, s"alias://${agent.id}/alias")
      threadSleep(15)
    }
    println("Agent labels complete")
  }

  def makeCnxns( dataset : DataSetDesc, chunkSize : Int = 4 ) : Unit = {
    println("Creating connection introductions")
    dataset.cnxns.foreach { connection =>
      val sourceId = connection.src.replace("agent://","")
      val sourceAlias = aliasesById(sourceId)
      val sourceURI = sessionsById(sourceId).sessionURI
      val targetId = connection.trgt.replace("agent://","")
      val targetAlias = aliasesById(targetId)
      glosevalPost("beginIntroductionRequest", BeginIntroductionRequest(sourceURI, "alias", Connection(sourceAlias, targetAlias), Connection(targetAlias, sourceAlias)))
      threadSleep(15)
    }
    println("Random introduction connections complete")
  }

  def parseData(
    dataJsonFile : String = serviceDemoDataFile(),
    configJsonFile : String = serviceSystemLabelsFile()
  ) = {
    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    parse(dataJson).extract[DataSetDesc]
  }

  def fromFiles(
    dataJsonFile : String = serviceDemoDataFile(),
    configJsonFile : String = serviceSystemLabelsFile(),
    host : String = GLOSEVAL_HOST,
    chunkSize : Int = 4
  ) {
    println("Beginning import procedure")
    GLOSEVAL_HOST = host

    //val configJson = scala.io.Source.fromFile(configJsonFile).getLines.map(_.trim).mkString
    //val config = parse(configJson).extract[ConfigDesc]
    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val dataset = parse(dataJson).extract[DataSetDesc]
    val agents = dataset.agents


    /*println("Importing agents")
    agents.foreach { agent =>
      println(glosevalPost("createUserRequest", CreateUserRequest(createEmailAddress(agent.loginId), agent.pwd, Map("name" -> agent.firstName), true)))
    }
    println("Agents import complete")

    threadSleep(45)

    println("Confirming agent emails")
    agents.foreach { agent =>
      try {
        val emailConfirmId = Mailinator.getInboxMessages(MAILINATOR_KEY, createEmailUser(agent.loginId)).toList.filter(m => m.getFrom == GLOSEVAL_SENDER).sortBy(_.getSeconds_ago).head.getId
        Mailinator.getEmail(MAILINATOR_KEY, emailConfirmId).getEmailParts.toList.filter(p => p.getBody.contains("token is")).foreach { part =>
          val token = part.getBody.replace("Your token is: ", "").trim.stripLineEnd
          println(s"Found token for ${agent.loginId}: $token. Confirming....")
          println(glosevalPost("confirmEmailToken", ConfirmEmailRequest(token)))
          threadSleep(15)
        }
      } catch {
        case e: Exception => println(s"Skipping agent ${agent.loginId} because ${e.getMessage}")
      }
    }
    println("Agent emails confirmation complete")

    threadSleep(30)*/

    makeAgents( dataset )

    makeLabels( agentsBySession )

    makeCnxns( dataset )

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
