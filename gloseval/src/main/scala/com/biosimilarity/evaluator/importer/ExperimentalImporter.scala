package com.biosimilarity.evaluator.importer

import java.io.File
import java.net.URI
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.util.Timeout
import com.biosimilarity.evaluator.api.Connection
import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.biosimilarity.evaluator.importer.models._
import com.biosimilarity.evaluator.spray.client.ApiClient
import com.biosimilarity.evaluator.spray.client.ClientSSLConfiguration.clientSSLEngineProvider
import com.biosimilarity.evaluator.util._
import com.biosimilarity.evaluator.util.mongo.MongoQuery
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.slf4j.{Logger, LoggerFactory}
import spray.http.{HttpResponse, Uri}

import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

class ExperimentalImporter(host: Uri) extends ApiClient {

  private val defaultAlias = "alias"

  val logger: Logger = LoggerFactory.getLogger(classOf[ExperimentalImporter])

  val maxNumberOfPingUntilPongs = 5

  val timeoutLength: Int = EvalConfigWrapper.readIntOrElse("pongTimeout", 15) * (maxNumberOfPingUntilPongs + 1)

  val system = ActorSystem()
  implicit val ec = system.dispatcher

  implicit val timeout: Timeout = Timeout(FiniteDuration(timeoutLength, SECONDS))

  def createAgent(hc: ActorRef, agent: AgentDesc): Future[String] = {
    val jsonBlob = parse(agent.jsonBlob).extract[JObject]
    createUser(hc, host, agent.email, agent.pwd, jsonBlob)
  }

  def createSession(hc: ActorRef, email: String, pwd: String): Future[String] = {
    for {
      rsp <- openSRPSession(hc, host, email, pwd)
    } yield rsp.sessionURI
  }

  def makeLabel(label: LabelDesc, labels: Map[String,LabelDesc]): (LabelDesc, Map[String,LabelDesc] )= {

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

    var tlbls = labels
    val desc = label match {
      case smpl: SimpleLabelDesc =>
        smpl.id.foreach(s => tlbls = tlbls + ((s, smpl)))
        smpl
      case cmplx: ComplexLabelDesc =>
        val rslt = reorderComponents(cmplx)
        cmplx.id.foreach(s => tlbls = tlbls + ((s, rslt)))
        rslt
      case ref: LabelRef => ref
    }
    (desc,tlbls)
  }

  def makeAgent(hc: ActorRef, agent: AgentDesc, commonLabels: Map[String,LabelDesc]): Future[(AgentDesc,String)] = {
    val lbls: List[String] = agent.aliasLabels match {
      case None => Nil
      case Some(l) =>
        val (_, l2) =
          l.foldLeft[(Map[String,LabelDesc],List[String])] ( (commonLabels, Nil) )( (pr, lbl) => {
            val labels: Map[String,LabelDesc] = pr._1
            val (desc,newlbls) = makeLabel(LabelDesc.extractFrom(lbl),labels)
            val lst = desc.toTermString(labels) :: pr._2
            (newlbls,lst)
        })
        l2
    }
    for {
      agentURI <- createAgent(hc, agent)
      ssn <- createSession(hc, agent.email, agent.pwd)
      _ <- addAliasLabels(hc, host, ssn, defaultAlias, lbls)
      _ <- closeSession(hc, host, ssn)
    } yield (agent, agentURI.replace("agent://cap/", "").slice(0, 36))
  }

  def makeConnectionLabels(cnxns: List[ConnectionDesc], agentsById: Map[String,String]): Map[(String,String),String] = {
    cnxns.foldLeft( Map.empty[(String,String), String] )( (cnxnLabels, connection) => {
      val sourceId: String = agentsById(connection.src.replace("agent://", ""))
      val targetId: String = agentsById(connection.trgt.replace("agent://", ""))
      if (!cnxnLabels.contains((sourceId,targetId))) {
        val cnxnLabel = UUID.randomUUID().toString
        cnxnLabels + ( ((sourceId,targetId), cnxnLabel), ((targetId,sourceId), cnxnLabel ) )
      }
      else cnxnLabels
    })
  }

  def makeConnections(hc: ActorRef, cnxnLabels: Map[(String,String),String], ssn: String, agentsById: Map[String,String]): Future[List[HttpResponse]] = {
    val futs = cnxnLabels.map( { case ((sourceId,targetId),cnxnLabel) =>
      makeConnection(hc, host, ssn, sourceId, targetId, cnxnLabel)
    }).toList
    Future.sequence(futs)
  }

  def createPosts(hc: ActorRef, posts: List[PostDesc], agentsById: Map[String,String], agentDescsById: Map[String,AgentDesc], cnxnLabels: Map[(String,String),String]): Future[List[HttpResponse]] = {
    Future.sequence( posts.map( post => {
      val sourceId = agentsById(post.src)
      val agent = agentDescsById(post.src)
      val sourceAlias = makeAliasUri(sourceId, defaultAlias)
      val cnxns: List[Connection] = post.trgts.map(t => {
        val trgt = agentsById(t)
        val lbl = cnxnLabels((sourceId,trgt))
        val trgtAlias = makeAliasUri(trgt, defaultAlias)
        Connection(sourceAlias, trgtAlias, lbl)
      })
      for {
        ssn <- createSession(hc, agent.email, agent.pwd)
        rsp <- makePost(hc, host, ssn, cnxns, post.label, post.value, post.uid)
        _ <- closeSession(hc, host, ssn)
      } yield rsp
    }))
  }

  def printStats(): Unit = {
    val qry = new MongoQuery()
    qry.printAliasCnxns()
  }

  def importData(dataJson: String, email: String, password: String): Future[Unit] = {
    val dataset = parse(dataJson).extract[DataSetDesc]

    def makeLabels(): Map[String,LabelDesc] = dataset.labels match {
      case Some(l) =>
        val (dic,_) =
          l.foldLeft[(Map[String,LabelDesc],List[String])] ( (Map.empty[String,LabelDesc], Nil) )( (pr, lbl) => {
            val labels: Map[String,LabelDesc] = pr._1
            val (desc,newlbls) = makeLabel(LabelDesc.extractFrom(lbl),labels)
            val lst = desc.toTermString(labels) :: pr._2
            (newlbls,lst)
          })
        dic
      case None => Map.empty[String,LabelDesc]
    }

    def makeCnxnLabels(agentsById: Map[String, String]): Map[(String,String), String] = {
      dataset.cnxns match {
        case Some(cnxns) => makeConnectionLabels(cnxns, agentsById)
        case None => Map.empty[(String,String), String]
      }
    }

    def importPosts(hc: ActorRef, ssn: String, agentsById: Map[String, String], agentDescs: Map[String, AgentDesc], cxnxLabels: Map[(String,String), String]): Future[List[HttpResponse]] = {
      dataset.posts match {
        case Some(posts) => createPosts(hc, posts, agentsById, agentDescs, cxnxLabels)
        case None => Future {
          Nil
        }
      }
    }

    def makeAgents(hc: ActorRef, labels: Map[String,LabelDesc]): Future[List[(AgentDesc, String)]] = {
      Future.sequence(dataset.agents.map(makeAgent(hc,_,labels)))
    }

    def makeAgentsById(prs: List[(AgentDesc, String)]): Map[String, String] = {
      val tprs: List[(String, String)] = prs.map( pr => (pr._1.id, pr._2))
      Map(tprs: _*)
    }

    def makeAgentDescsById(prs: List[(AgentDesc, String)]): Map[String, AgentDesc] = {
      val tprs: List[(String, AgentDesc)] = prs.map(pr => (pr._1.id, pr._1))
      Map(tprs: _*)
    }

    val lbls = makeLabels()
    for {
      hc <- eventualHostConnector(system, host.effectivePort, clientSSLEngineProvider)
      adminSession <- createSession(hc, email, password)
      prs <- makeAgents(hc, lbls)
      agentsById = makeAgentsById(prs)
      descs = makeAgentDescsById(prs)
      cnxnLabels = makeCnxnLabels(agentsById)
      _ <- makeConnections(hc, cnxnLabels, adminSession, agentsById)
      _ <- importPosts(hc, adminSession, agentsById, descs, cnxnLabels)
      _ <- closeSession(hc, host, adminSession)
    } yield printStats()
  }

}

object ExperimentalImporter extends ApiClient {

  def importFile(dataJsonFile: File,
               host: URI = EvalConfigWrapper.serviceHostURI,
               email: String = EvalConfigWrapper.email,
               password: String = EvalConfigWrapper.password): Unit = {
    println(s"Importing file: $dataJsonFile")
    val dataJson: String = scala.io.Source.fromFile(dataJsonFile).getLines.map(_.trim).mkString
    val uri = Uri(host.toString)
    val imp = new ExperimentalImporter(uri)
    implicit val ec: ExecutionContext = imp.ec
    val rslt = imp.importData(dataJson, email, password)
    rslt.onComplete {
      case Success(_) => println(s"File import completed successfully.")
      case Failure(e) => e.printStackTrace()
    }
    Await.ready(rslt, Duration.Inf)
  }

  def fromTestData(testDataFilename: String = EvalConfigWrapper.serviceDemoDataFilename,
                   host: URI = EvalConfigWrapper.serviceHostURI,
                   email: String = EvalConfigWrapper.email,
                   password: String = EvalConfigWrapper.password): Unit =
    importFile(testDir.resolve(s"$testDataFilename.json").toFile, host, email, password)
}
