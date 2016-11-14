package com.biosimilarity.evaluator.importer

import java.io.File
import java.net.URI
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.biosimilarity.evaluator.api.Connection
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.spray.Server
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration.clientSSLEngineProvider
import com.biosimilarity.evaluator.util._
import com.biosimilarity.evaluator.util.mongo.MongoQuery
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods._
import org.json4s.{BuildInfo => _}
import org.slf4j.{Logger, LoggerFactory}
import spray.http.Uri

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ExperimentalImporter(host: Uri) extends ApiClient {

  // maps loginId to agentURI
  private val agentsById = scala.collection.mutable.Map[String, Future[String]]()

  // maps loginId to agentDesc
  private val agentDescsById = scala.collection.mutable.Map[String, AgentDesc]()

  // maps src+trgt to label
  private val cnxnLabels = scala.collection.mutable.Map[String, String]()

  private val labels = scala.collection.mutable.Map[String, LabelDesc]()
  private val defaultAlias = "alias"

  private def resolveLabel(id: String): LabelDesc = labels(id)

  val logger: Logger = LoggerFactory.getLogger(classOf[ExperimentalImporter])

  val maxNumberOfPingUntilPongs = 5

  val timeoutLength: Int = EvalConfigWrapper.readIntOrElse("pongTimeout", 15) * (maxNumberOfPingUntilPongs + 1)

  val system = ActorSystem()
  implicit val ec = system.dispatcher

  implicit val timeout: Timeout = Timeout(FiniteDuration(timeoutLength, SECONDS))

  val ehc: Future[ActorRef] = eventualHostConnector(system, host.effectivePort, clientSSLEngineProvider)

  def createAgent(agent: AgentDesc): Future[String] = {
    val jsonBlob = parse(agent.jsonBlob).extract[JObject]
    for {
      hc <- ehc
      usr <- createUser(hc, host, agent.email, agent.pwd, jsonBlob)
    } yield usr
  }

  def createSession(email: String, pwd: String): Future[String] = {
    for {
      hc <- eventualHostConnector(system, host.effectivePort, clientSSLEngineProvider)
      rsp <- openSRPSession(hc, host, email, pwd)
    } yield rsp.sessionURI
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
        case ComplexLabelDesc(id, "leaf", lbls) =>
          val (tp, r) = lbls.partition(matchFunctor("text", _))
          if (tp.length > 1) throw new Exception("label must contain at most one text field")
          val (dp, r2) = r.partition(matchFunctor("display", _))
          if (dp.length > 1) throw new Exception("label must contain at most one display field")
          val lbls2 = tp ++ dp ++ r2
          ComplexLabelDesc(id, "leaf", lbls2)
        case _ => lbl
      }
    }

    label match {
      case smpl: SimpleLabelDesc =>
        smpl.id.foreach(s => labels.put(s, smpl))
        smpl
      case cmplx: ComplexLabelDesc =>
        val rslt = reorderComponents(cmplx)
        cmplx.id.foreach(s => labels.put(s, rslt))
        rslt
      case ref: LabelRef => ref //labels(ref.label)  // throw if not present??
    }

  }

  def makeAgent(agent: AgentDesc): Unit = {
    for {
      hc <- ehc
      agentURI <- createAgent(agent)
      ssn <- createSession(agent.email, agent.pwd)
    } {
      val agentCap = agentURI.replace("agent://cap/", "").slice(0, 36)
      agentsById.put(agent.id, Future(agentCap))
      agentDescsById.put(agent.id, agent)
      agent.aliasLabels match {
        case None =>()
        case Some(l) =>
          val lbls: List[String] = l.map(lbl => makeLabel(LabelDesc.extractFrom(lbl)).toTermString(resolveLabel))
          addAliasLabels(hc, host, ssn, defaultAlias, lbls)
      }
      closeSession(hc, host, ssn)
    }
  }

  def makeCnxn(sessionId: String, connection: ConnectionDesc): Unit = {
    for {
      hc <- ehc
      sourceId <- agentsById(connection.src.replace("agent://", ""))
      targetId <- agentsById(connection.trgt.replace("agent://", ""))
    } {
      val cnxnLabel = UUID.randomUUID().toString
      if (!cnxnLabels.contains(sourceId + targetId)) {
        makeConnection(hc, host, sessionId, sourceId, targetId, cnxnLabel)
        cnxnLabels.put(sourceId + targetId, cnxnLabel)
        cnxnLabels.put(targetId + sourceId, cnxnLabel)
      }
    }
  }

  def createPost(post: PostDesc): Unit = {
    for {
      hc <- ehc
      sourceId <- agentsById(post.src)
      agent = agentDescsById(post.src)
      ssn <- createSession(agent.email, agent.pwd)
      targets <- Future.sequence(post.trgts.map(t => agentsById(t)))
    } {
      var cnxns: List[Connection] = Nil
      val sourceAlias = makeAliasUri(sourceId, defaultAlias)

      targets.foreach(trgt => {
        val lbl = cnxnLabels(sourceId + trgt)
        val trgtAlias = makeAliasUri(trgt, defaultAlias)
        cnxns = Connection(sourceAlias, trgtAlias, lbl) :: cnxns
      })

      makePost(hc, host, ssn, cnxns, post.label, post.value,post.uid )
      closeSession(hc, host, ssn)
    }
  }
  def printStats(): Unit = {
    val qry = new MongoQuery()
    qry.printAliasCnxns()
  }

  def importData(dataJson: String, email: String, password: String): Future[Int] = {
    val dataset = parse(dataJson).extract[DataSetDesc]
    var rslt: Int = 0
    for {
      //hc <- ehc
      adminSession <- createSession(email, password)
    } yield {
      try {
        dataset.labels match {
          case Some(lbls) => lbls.foreach(l => {
            makeLabel(LabelDesc.extractFrom(l))
          })
          case None => ()
        }
        dataset.agents.foreach(a => {
          makeAgent(a)
        })
        dataset.cnxns match {
          case Some(cnxns) => cnxns.foreach(cnxn => {
            makeCnxn(adminSession, cnxn)
          })
          case None => ()
        }
        dataset.posts match {
          case Some(posts) => posts.foreach(p => {
            createPost(p)
          })
          case None => ()
        }
      } catch {
        case ex: Throwable =>
          println("ERROR : " + ex)
          rslt = 1
      } finally {
        //closeSession(hc, host, adminSession)
      }
      rslt
    }
  }
}

object ExperimentalImporter extends ApiClient {

  def importFile(dataJsonFile: File,
               host: URI = EvalConfigWrapper.serviceHostURI,
               email: String = EvalConfigWrapper.email,
               password: String = EvalConfigWrapper.password): Future[Int] = {
    println(s"Importing file: $dataJsonFile")
    val dataJson: String = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val uri = Uri(host.toString)
    val imp = new ExperimentalImporter(uri)
    implicit val ec: ExecutionContext = imp.ec
    for {
      rslt <- imp.importData(dataJson, email, password)
    } yield {
      println("Import file returning : " + rslt)
      if (rslt == 0) imp.printStats()
      rslt
    }
  }

  def fromTestData(testDataFilename: String = EvalConfigWrapper.serviceDemoDataFilename,
                   host: URI = EvalConfigWrapper.serviceHostURI,
                   email: String = EvalConfigWrapper.email,
                   password: String = EvalConfigWrapper.password): Future[Int] =
    importFile(testDir.resolve(s"$testDataFilename.json").toFile, host, email, password)
}
