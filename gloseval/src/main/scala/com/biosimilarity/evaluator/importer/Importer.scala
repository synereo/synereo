// -*- mode: Scala;-*- 
// Filename:    Importer.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jan 19 16:49:16 2016 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.importer

import java.util.UUID

import com.biosimilarity.evaluator.distribution.EvalConfig
import com.biosimilarity.evaluator.importer.dtos._
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.spray.NodeUser
import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import java.util.UUID

import scalaj.http.Http

/**
 * Iterates through a sample data file, parses it, and imports the data
 *
 * @note To run the importer, ensure you are using a recent version of Maven.
 * @note First run `mvn clean compile` in the terminal.
 * @note Second run `mvn scala:console`.
 * @note Finally, import this object and execute `Importer.fromFiles()`. You may need to set the file paths in the parameters.
  * @note - it may also be required to reset mongo db with ' mongo records --eval "db.dropDatabase()" '
 */
object Importer extends EvalConfig
  with ImporterConfig
  with Serializable {

  implicit val formats = org.json4s.DefaultFormats

  private var GLOSEVAL_HOST = serviceHostURI()
  //private val GLOSEVAL_SENDER = serviceEmailSenderAddress()
  //private val MAILINATOR_HOST = serviceMailinatorHost()
  //private val MAILINATOR_KEY = serviceMailinatorKey()

  private val agentsById = scala.collection.mutable.Map[String, String]() // loginId:agentURI
  private val agentsBySession = scala.collection.mutable.Map[String, AgentDesc]() // sessionURI:agent
  private val sessionsById = scala.collection.mutable.Map[String, InitializeSessionResponse]() // sessionURI:agent
  private val aliasesById = scala.collection.mutable.Map[String, String]() // agentId:aliasUri

  private def glosevalPost(msgType: String, data: RequestContent): String = {
    val requestBody = write(ApiRequest(msgType, data))
    println(s"REQUEST: ${msgType}")
    println(s"REQUEST BODY: ${requestBody}")

    val req = Http(GLOSEVAL_HOST)
      .timeout(1000, 60000)
      .header("Content-Type", "application/json")
      .postData(requestBody)
    val response = req.asString.body

    println(s"RESPONSE BODY: ${response}")
    response
  }

  def makeAliasURI(alias: String) = s"alias://$alias/alias"

  private def makeAliasLabel(label: String, color: String) = s"""leaf(text("${label}"),display(color("${color}"),image("")))"""

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
                      case "connectionProfileResponse" => {}
                      case "addAliasLabelsResponse" => {}
                      case "beginIntroductionResponse" => {}
//                        println("beginIntroductionResponse : " + (v \ "content"))
//                        println("WARNING - handler not provided for server sent message type : " + typ)
//                      }
                      case _ => {
                        //println("contents : " + (v \ "content"))
                        println("WARNING - handler not provided for server sent message type : " + typ)
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
          println("longpoll sleeping")
          Thread.sleep(10000)
        }
      }
    })
  }

  def createAgent(agent: AgentDesc) : Option[String] = {
    val eml = agent.email + (if (agent.email.contains("@")) "" else "@livelygig.com")

    val json1 = glosevalPost(
      "createUserRequest",
      CreateUserRequest(
        "noConfirm:" + eml,
        agent.pwd,
        agent.jsonBlob
      )
    )

    val jsv = parse(json1)

    val tmsg = (jsv \ "msgType").extract[String]
    if (tmsg == "createUserError") {
      println("create user failed, reason : " + (jsv \ "content" \ "reason").extract[String])
      None
    }
    else {
      val agentURI = (jsv \ "content" \ "agentURI").extract[String]
      Some(agentURI)
    }
  }

  def createSession( agentURI : String, pwd : String) : InitializeSessionResponse = {
    val json =
      glosevalPost(
        "initializeSessionRequest",
        InitializeSessionRequest(agentURI + "?password=" + pwd))

    val jsession: JValue = (parse(json) \ "content")
    val uri = (jsession \ "sessionURI").extract[String]
    val alias = (jsession \ "defaultAlias").extract[String]

    InitializeSessionResponse(uri, agentURI, alias, jsession)
  }

  def makeAgent(agent: AgentDesc): Unit = {
    createAgent(agent) match {
      case None => ()
      case Some(agentURI) =>
        val agentId = agentURI.replace("agent://", "")
        agentsById.put(agent.id, agentId)
        val session = createSession(agentURI, agent.pwd)
        agentsBySession.put(session.sessionURI, agent)
        sessionsById.put(agent.id, session)

        //@@GS - what exactly is this intended to achieve??
        glosevalPost("addAliasLabelsRequest", AddAliasLabelsRequest(session.sessionURI, "alias", List(makeAliasLabel(agentId, "#5C9BCC"))))

        aliasesById.put(agent.id, s"alias://${agentId}/alias")
    }
  }

  def openAdminSession() = {
    val admin = AgentDesc("", NodeUser.email, NodeUser.password, "{}" )
    val sess =
      createAgent(admin) match {
        case None => throw new Exception("Unable to open admin session")
        case Some(uri) => createSession(uri, NodeUser.password)
      }
    sess
  }

  def makeLabel(label : SystemLabelDesc): Unit = {
    /*
    try {
    } catch {
      case ex: Throwable => println("exception while creating connection: " + ex)
    }
    */
  }

  def makeCnxn(adminURI : String, connection : ConnectionDesc): Unit = {
    try {
      val sourceId = connection.src.replace("agent://","")
      val sourceAlias = aliasesById(sourceId)
      val targetId = connection.trgt.replace("agent://","")
      val targetAlias = aliasesById(targetId)
      val lbl: String = connection.label match {
        case Some(LabelDesc(_, value, _)) => value
        case None => "cnxn" //UUID.randomUUID.toString()
      }
      val aMessage = ""
      val bMessage = ""
      glosevalPost("beginIntroductionRequest", BeginIntroductionRequest(adminURI, "alias", Connection(sourceAlias, targetAlias, lbl), Connection(targetAlias, sourceAlias, lbl), aMessage, bMessage))
    } catch {
      case ex: Throwable => println("exception while creating connection: " + ex)
    }
  }

  def makePost(post : PostDesc): Unit = {
    try {
      var cnxns : List[Connection] = Nil

      val sourceId = post.src
      val sourceAlias = aliasesById(sourceId)
      val sourceURI = sessionsById(sourceId).sessionURI

      val agentURI = "agent://" + sourceAlias.replace("alias://","").replace("/alias","")
      cnxns = Connection(agentURI, agentURI, "alias") :: cnxns

      post.trgts.foreach(trgt => {
        val lbl = UUID.randomUUID.toString()
        val trgtAlias = aliasesById(trgt)
        cnxns = Connection(sourceAlias, trgtAlias, lbl) :: cnxns
      })

      val uid = UUID.randomUUID.toString()
      val lbl = post.label // maybe later: .labels.mkString("[",",","]")
      val cont = EvalSubscribeContent(cnxns, lbl, post.value, uid)

      glosevalPost("evalSubscribeRequest", EvalSubscribeRequest(sourceURI, EvalSubscribeExpression("insertContent", cont)))
    } catch {
      case ex: Throwable => println("exception while creating post: " + ex)
    }
  }

  def parseData( dataJsonFile: String = serviceDemoDataFile() ) = {
    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    parse(dataJson).extract[DataSetDesc]
  }

  def fromFile(
                   dataJsonFile: String,
                   host: String = GLOSEVAL_HOST ) : Unit = {

    println("Importing file : " +  dataJsonFile)
    GLOSEVAL_HOST = host

    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val dataset = parse(dataJson).extract[DataSetDesc]
    val adminSession = openAdminSession()

    val thrd = longPoll()
    thrd.start()

    try {
      dataset.agents.foreach(makeAgent)

      dataset.labels match {
        case Some(lbls) => lbls.foreach (makeLabel)
        case None => ()
      }

      dataset.cnxns match {
        case Some(cnxns) => cnxns.foreach(cnxn => makeCnxn(adminSession.sessionURI, cnxn))
        case None => ()
      }

      dataset.posts match {
        case Some(posts) => posts.foreach(makePost)
        case None => ()
      }
    } finally {
      // need to fix this
      // wait ten seconds for long poll receipts
      thrd.interrupt()

      thrd.join(10000)
    }
  }

  def fromFiles(
                 dataJsonFile: String = serviceDemoDataFile(),
                 host: String = GLOSEVAL_HOST ): Unit = {
    fromFile(dataJsonFile, host)
    //fromFile("src/main/resources/test-posts.json", host)
  }


}
