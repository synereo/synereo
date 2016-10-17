package com.biosimilarity.evaluator.util.mongo

import com.biosimilarity.evaluator.distribution.EvalConfigWrapper
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI
import org.json4s.{JArray, JString}
import org.json4s.JsonAST.{JObject, JValue}
import org.json4s.jackson.JsonMethods._

object MongoQuery {
  val defaultHost = EvalConfigWrapper.readStringOrElse("dbHost", "localhost")
  val defaultPort = EvalConfigWrapper.readStringOrElse("dbPort", "27017")
}

case class AliasCnxnContent(posts: List[JArray], labels: List[String], cnxns: List[JObject], orphans: List[JObject], biCnxnBouncers: List[String], errs: List[(JValue,Throwable)])

class MongoQuery(dbHost: String = MongoQuery.defaultHost, dbPort: String = MongoQuery.defaultPort) {
  implicit val formats = org.json4s.DefaultFormats

  val uri = MongoClientURI(s"mongodb://$dbHost:$dbPort/")
  val mongoClient: MongoClient = MongoClient(uri)
  val records: MongoDB = mongoClient("records")

  def readAllIdentities(): List[(String, String)] = {
    var rslt : List[(String,String)] = Nil
    val ids = records.collectionNames().filter( nm => nm.contains("identity") )
    ids.foreach( id => {
      val i = id.indexOf("identity")
      val uid = id.substring(0,i)
      if (uid == id.substring(i+"identity".length())) {
        val coll = records(id)
        var bncrs : List[DBObject] = Nil
        var blobs : List[String] = Nil
        for (dbo <- coll) {
          val rec = dbo.get("record").asInstanceOf[DBObject]
          val key = rec.get("key").asInstanceOf[DBObject].head._1.asInstanceOf[String]
          val v = rec.get("value").asInstanceOf[DBObject].head._1.asInstanceOf[String]
          if (key == "jsonBlob") {
            blobs = v :: blobs
          } else if (key == "email") {
          } else if (key == "pwd") {
          } else if (key == "aliasList") {
          } else if (key == "defaultAlias") {
          } else {
            bncrs = rec :: bncrs
          }

        }
        bncrs.foreach( rec => {
          val key = rec.get("key").asInstanceOf[DBObject].head._1.asInstanceOf[String]
          blobs.find( _ contains key ) match {
            case Some(r) => {
              val js = rec.get("value").asInstanceOf[DBObject].head._1.asInstanceOf[String]
              val jo = parse(js)
              val JString(s) = jo.children(0).children(0).children(1).children(1).asInstanceOf[JString]
              val blob = parse(s)
              val nm = (blob \ "name").extract[String]
              rslt = (uid, nm ) :: rslt
            }
            case None => ()
          }

        })

      }
    })
    rslt
  }

  def readAliasCnxnContent(agent: String): AliasCnxnContent = {
    //DANGER WILL ROBINSON DANGER ...
    //this code is absolutely horrible, and uses fragile heuristics to attempt to
    //extract the relevant data based on how it currently appears
    val id = agent + "alias" + agent
    val coll: MongoCollection = records(id)
    val csr = coll.find("record" $exists true)
    var posts: List[JArray] = Nil
    var errs: List[(JValue, Throwable)] = Nil
    var labels: List[String] = Nil
    var cnxns: List[(String,JObject)] = Nil
    var biCnxnBouncers: List[String] = Nil

    for (dbo <- csr) {
      try {
        val s = dbo.toString
        if (s.contains("\"biCnxnsList\"")) biCnxnBouncers = s :: biCnxnBouncers
        else if (s.contains("ConcreteHL$PostedExpr")) {
          val tdb = dbo.get("record").asInstanceOf[DBObject].get("value").asInstanceOf[DBObject]
          val js = tdb.head._1
          val jo = parse(js)
          try {
            val JString(s) = jo.children(0).children(0).children(1).children(1).asInstanceOf[JString]
            val ta = parse(s).extract[JArray]
            if (ta(0).isInstanceOf[JString]) {
              ta.children.foreach(tv => {
                val JString(slbl) = tv.asInstanceOf[JString]
                labels = slbl :: labels
              })
            }
            else {
              val tjo: JObject = ta(0).asInstanceOf[JObject]
              if (tjo.values.contains("readCnxn")) {
                val bnckey = dbo.get("record").asInstanceOf[DBObject].get("key").asInstanceOf[DBObject].head._1.asInstanceOf[String]
                ta.children.foreach(tv => {
                  cnxns = (bnckey,tv.asInstanceOf[JObject]) :: cnxns
                })
              }
              else {
                if (tjo.values.contains("$type"))
                  posts = ta :: posts
                else
                  throw new Exception("Unable to classify content")
              }
            }
          }
          catch {
            case e: Throwable =>
              errs = (jo, e) :: errs
          }
        }
      }
      catch {
        case e: Throwable =>
          errs = (new JString("wtf"), e) :: errs
      }
    }
    val (orphans,good) = cnxns.partition( pr => biCnxnBouncers.count( s => s.contains(pr._1)) == 0)
    AliasCnxnContent(posts, labels, good.map( _._2), orphans.map(_._2), biCnxnBouncers, errs)
  }

  def readAllAliasCnxns() : List[(String,AliasCnxnContent)] = {
    val ids: List[(String, String)] = readAllIdentities()
    ids.map( pr => (pr._2, readAliasCnxnContent(pr._1)) )
  }


}
