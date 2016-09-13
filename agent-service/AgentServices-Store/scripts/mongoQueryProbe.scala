// -*- mode: Scala;-*-
// Filename:    mongoQueryProbe.scala
// Authors:     lgm
// Creation:    Tue Aug  6 14:24:40 2013
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

object MongoQueryProbe {

  import com.biosimilarity.evaluator.distribution._
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope.Being._
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor._
  import com.biosimilarity.lift.model.store._
  import com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper
  import com.mongodb.casbah.Imports._

  def erqlInstance(): CnxnCtxtLabel[String, String, String] =
    DSLCommLinkCtor.ExchangeLabels
      .evalRequestLabel()(Left[String, String](java.util.UUID.randomUUID().toString))
      .getOrElse(throw new Exception("error making evalRequestLabel"))

  def stdEvalChannel: Option[StdEvalChannel] = EvalNodeMapper.headOption.map(_._2)

  def evalRequestLabel(s: String) = DSLCommLinkCtor.ExchangeLabels.evalRequestLabel()(Right(s))

  def mongoDBManifest: Option[MongoDBManifest] = stdEvalChannel.flatMap { (channel: StdEvalChannel) =>
    channel.cache.persistenceManifest match {
      case Some(mpm: MongoDBManifest) => Some(mpm)
      case _                          => None
    }
  }

  def kQuery(channel: StdEvalChannel, collectionName: String, ptn: CnxnCtxtLabel[String, String, String]): Option[DBObject] =
    mongoDBManifest.flatMap { (manifest: MongoDBManifest) =>
      manifest.kqueryDBObject(collectionName, ptn)
    }
}
