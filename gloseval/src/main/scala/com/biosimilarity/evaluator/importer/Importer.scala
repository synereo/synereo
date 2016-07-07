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
import org.json4s.JsonAST.{JArray, JValue, JObject}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write
import org.json4s.JsonDSL._
import java.util.UUID

import com.biosimilarity.evaluator.omniRPC.{OmniClient, OmniConfig}

import spray.http.DateTime

import scalaj.http.Http

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

  private var GLOSEVAL_HOST = serviceHostURI()
  //private val GLOSEVAL_SENDER = serviceEmailSenderAddress()
  //private val MAILINATOR_HOST = serviceMailinatorHost()
  //private val MAILINATOR_KEY = serviceMailinatorKey()

  private val agentsById = scala.collection.mutable.Map[String, String]()
  // loginId:agentURI
  private val sessionsById = scala.collection.mutable.Map[String, InitializeSessionResponse]()
  // sessionURI:agent
  private val cnxnLabels = scala.collection.mutable.Map[String, String]() // src+trgt:label

  private val labels = scala.collection.mutable.Map[String, LabelDesc]() // id

  private def resolveLabel(id: String): LabelDesc = labels(id)

  private def glosevalPost(msgType: String, data: RequestContent): String = {
    println(s"REQUEST: ${msgType}")
    val requestBody = write(ApiRequest(msgType, data))
    glosevalPost(requestBody)

  }


  private def glosevalPost(msgType: String, data: JValue): String = {
    println(s"REQUEST: ${msgType}")
    val requestBody = write( ("msgType" -> msgType) ~ ("content" -> data) )
    glosevalPost(requestBody)

  }

  private def glosevalPost(requestBody: String): String = {
    println(s"REQUEST BODY: ${requestBody}")

    val req = Http(GLOSEVAL_HOST)
      .timeout(1000, 60000)
      .header("Content-Type", "application/json")
      .postData(requestBody)
    val response = req.asString.body

    println(s"RESPONSE BODY: ${response}")
    if (response.startsWith("Malformed request")) throw new Exception(response)
    response
  }

  def makeAliasURI(alias: String) = s"alias://${alias}/alias"

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
          val tmp = sessionsById.clone()
          while (tmp.nonEmpty) {
            tmp.foreach {
              case (id, sess) =>
                try {
                  println("Sending Ping")
                  val session = sess.sessionURI
                  val js = glosevalPost("sessionPing", SessionPingRequest(session))
                  val arr = parse(js).extract[List[JValue]]
                  arr.foreach(v => {
                    val typ = (v \ "msgType").extract[String]
                    typ match {
                      case "sessionPong" => tmp.remove(id)
                      case "connectionProfileResponse" => {
                        //println("connectionProfileResponse : " + (v \ "content"))

                      }
                      case "addAliasLabelsResponse" => {}
                      case "beginIntroductionResponse" => {
                        //println("beginIntroductionResponse : " + (v \ "content"))
                      }
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

  def expect(msgType: String, session: String): Option[JValue] = {
    println("Sending Ping")
    val js = glosevalPost("sessionPing", SessionPingRequest(session))
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
    val json1 = glosevalPost(
      "createUserRequest",
      CreateUserRequest(
        "noConfirm:" + eml,
        agent.pwd,
        jsonBlob
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

  def createSession(agentURI: String, pwd: String): InitializeSessionResponse = {
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
        val agentCap = agentURI.replace("agent://cap/", "").slice(0, 36)
        agentsById.put(agent.id, agentCap)
        val session = createSession(agentURI, agent.pwd)
        sessionsById.put(agent.id, session)

        agent.aliasLabels match {
          case None =>()
          case Some(l) =>
            val lbls = l.map(lbl => makeLabel(LabelDesc.extractFrom(lbl)).toTermString(resolveLabel))
            glosevalPost("addAliasLabelsRequest", AddAliasLabelsRequest(session.sessionURI, "alias", lbls))
        }
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
        //glosevalPost("beginIntroductionRequest", BeginIntroductionRequest(adminURI, "alias", Connection(sourceAlias, targetAlias, lbl), Connection(targetAlias, sourceAlias, lbl), aMessage, bMessage))
        glosevalPost("establishConnectionRequest", EstablishConnectionRequest(sessionId, sourceURI, targetURI, cnxnLabel))
        cnxnLabels.put(sourceId + targetId, cnxnLabel)
        cnxnLabels.put(targetId + sourceId, cnxnLabel)
      }
    } catch {
      case ex: Throwable => println("exception while creating connection: " + ex)
    }
  }

  def makePost(post: PostDesc): Unit = {
    try {
      var cnxns: List[Connection] = Nil

      val sourceId = agentsById(post.src)
      val sourceAlias = makeAliasURI(sourceId)
      val sourceSession = sessionsById(post.src).sessionURI

      val selfcnxn = Connection("agent://" + sourceId, "agent://" + sourceId, "alias")
      //val selfcnxn = Connection(sourceAlias, sourceAlias, "alias")

      post.trgts.foreach(trgt => {
        val targetId = agentsById(trgt)
        val lbl = cnxnLabels(sourceId + targetId)
        val trgtAlias = makeAliasURI(agentsById(trgt))
        cnxns = Connection(sourceAlias, trgtAlias, lbl) :: cnxns
      })

      val cont = EvalSubscribeContent(selfcnxn :: cnxns, post.label, post.value, post.uid)

      glosevalPost("evalSubscribeRequest", EvalSubscribeRequest(sourceSession, EvalSubscribeExpression("insertContent", cont)))
    } catch {
      case ex: Throwable => println("exception while creating post: " + ex)
    }
  }

  def parseData(dataJsonFile: String = serviceDemoDataFile()) = {
    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    parse(dataJson).extract[DataSetDesc]
  }

  def getAgentURI(email: String, password: String) = {
    val json = glosevalPost("getAgentRequest", GetUserRequest(email, password))
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

    println("Importing file : " + dataJsonFile)
    GLOSEVAL_HOST = host

    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val dataset = parse(dataJson).extract[DataSetDesc]

    getAgentURI(NodeUser.email, NodeUser.password) match {
      case Some(uri) => {
        val adminId = uri.replace("agent://", "")
        val adminSession = createSession(uri, NodeUser.password)
        sessionsById.put(adminId, adminSession) // longpoll on adminSession
        println("using admin session URI : " + adminSession.sessionURI)
        //val thrd = longPoll()
        //thrd.start()

        try {
          dataset.labels match {
            case Some(lbls) => lbls.foreach(l => makeLabel(LabelDesc.extractFrom(l)))
            case None => ()
          }

          dataset.agents.foreach(makeAgent)

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
          //thrd.interrupt()

          //thrd.join(20000)
        }
      }
      case _ => throw new Exception("Unable to open admin session")
    }
  }

  def runTestFile(dataJsonFile: String, host: String = GLOSEVAL_HOST): Unit = {

    println("testing file : " + dataJsonFile)
    GLOSEVAL_HOST = host

    val dataJson = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val tests = parse(dataJson).extract[List[JObject]]
    val adminURI =
      getAgentURI(NodeUser.email, NodeUser.password) match {
        case Some(uri) => uri
        case _ => throw new Exception("unable to open admin session")
      }
    val adminId = adminURI.replace("agent://", "")
    val adminSession = createSession(adminURI, NodeUser.password)
    sessionsById.put(adminId, adminSession) // longpoll on adminSession
    println("using admin session URI : " + adminSession.sessionURI)
    //val thrd = longPoll()
    //thrd.start()
    var testOmni = OmniConfig.isOmniRequired()

    tests.foreach(el => {
      val typ = (el \ "type").extract[String]

      try {
        typ match {
          case "reset" => {
            val isok = glosevalPost("resetDatabaseRequest", ResetDatabaseRequest(adminSession.sessionURI, mongodbPath()))
            if (isok != "OK") throw new Exception("Unable to reset database")
            expect("resetDatabaseResponse", adminSession.sessionURI) match {
              case Some(_) => ()
              case _ => throw new Exception("Unable to reset database")
            }
          }
          case "agent" => {
            val agent = (el \ "content").extract[AgentDesc]
            makeAgent(agent)
            if (testOmni) {
              val session = sessionsById(agent.id).sessionURI
              val isok = glosevalPost("getAmpWalletAddress", ("sessionURI" -> session))
              if (isok != "OK") throw new Exception("Unable to call getAmpWalletAddress")
              expect("getAmpWalletAddressResponse", session) match {
                case Some(js) => {
                  println(pretty(js))
                  val oldaddr = (js \ "address").extract[String]
                  val isok2 = glosevalPost("setAmpWalletAddress", ("sessionURI" -> session) ~ ("address" -> OmniClient.testAmpAddress))
                  if (isok2 != "OK") throw new Exception("Unable to call setAmpWalletAddress")
                  expect("setAmpWalletAddressResponse", session) match {
                    case Some(js) => {
                      println(pretty(js))
                      val newaddr = (js \ "newaddress").extract[String]
                      if (newaddr != OmniClient.testAmpAddress) throw new Exception("setAmpWalletAddressResponse invalid")
                      val isok3 = glosevalPost("setAmpWalletAddress", ("sessionURI" -> session) ~ ("address" -> oldaddr))
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
              testOmni = false  // once is enough ...
            }
          }
          case "cnxn" => {
            val cnxn = (el \ "content").extract[ConnectionDesc]
            makeCnxn(adminSession.sessionURI, cnxn)
          }
          case "label" => {
            val jo = (el \ "content").extract[JObject]
            val lbl = LabelDesc.extractFrom(jo)
            makeLabel(lbl)
          }
          case "post" => {
            val post = (el \ "content").extract[PostDesc]
            makePost(post)
          }
          case _ => throw new Exception("Unknown test element")
        }
      } finally {
        // need to fix this
        // wait ten seconds for long poll receipts
        //thrd.interrupt()

        //thrd.join(20000)
      }

      if (testOmni) OmniClient.runTests()

    })
  }

  def fromFiles(dataJsonFile: String = serviceDemoDataFile(), host: String = GLOSEVAL_HOST): Unit = {
    fromFile(dataJsonFile, host)
    //fromFile("src/main/resources/sample-data-test_2.json", host)
  }

  def runTestFiles(host: String = GLOSEVAL_HOST): Unit = {
    runTestFile("src/test/resources/test-posts.json", host)
  }
}
