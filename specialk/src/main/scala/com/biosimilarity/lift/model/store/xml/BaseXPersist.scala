package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store._
import org.basex.server._
import com.biosimilarity.lift.model._
import com.biosimilarity.lift.lib._
import org.basex.core._
import cmd._
import scala.collection._
import scala.xml._
import scala.List

trait BaseXPersist extends Persist[ClientSession]
with XMLStoreConfiguration
with Schema
{
  //  self: Journalist
  //    with Reporting   =>

  override def configFileName: Option[String] = None

  override def configurationDefaults: ConfigurationDefaults =
  {
    ApplicationDefaults.asInstanceOf[ConfigurationDefaults]
  }

  def clientSessionFromConfig: ClientSession =
  {
    new ClientSession(
      dbHost,
      dbPort.toInt,
      dbUser,
      dbPwd
    )
  }


  def checkIfDBExists(collectionName: String, leaveOpen: Boolean): Boolean =
  {
    val clientSession = clientSessionFromConfig
    try {
      clientSession.execute(new Open(collectionName))
      true
    }
    catch {
      case e: BaseXException => {
        false
      }
    }
    finally{
      if (!leaveOpen) {
        clientSession.execute(new Close())
      }
    }
  }
  def checkIfDBExistsAndCreateIfNot(collectionName: String, leaveOpen: Boolean): Boolean =
  {
    try {
      open(collectionName: String)
      true
    }
    catch {
      case e: BaseXException => {
        false
      }
    }
  }

  private def createDBIfMissing(collectionName: String): ClientSession =
  {
    val clientSession = clientSessionFromConfig
    try {      

      clientSession.execute(new Open(collectionName))
      clientSession
    }
    catch {
      case bxe : BaseXException => {
        createAndOpen(collectionName)
      }
    }

  }
  private def createAndOpen(collectionName: String): ClientSession =
  {
    synchronized {
      val clientSession = clientSessionFromConfig
      try {
        clientSession.execute(new Open(collectionName))
        clientSession
      }
      catch {
        case bxe : BaseXException => {

          val clientSessionRetry = clientSessionFromConfig
          try {
            val cs = create(clientSessionRetry, collectionName)
            cs
          }
          catch {
            case inrBxe : BaseXException => {
              inrBxe.printStackTrace
              clientSessionRetry.execute(new Close())
              clientSessionRetry
            }
          }
        }
      }
    }
  }

  //open and create if missing
  def open(collectionName: String): ClientSession =
  {
    createDBIfMissing(collectionName)
  }

  def create(cs: ClientSession, collectionName: String): ClientSession =
  {
    try {
      //transaction
      cs.execute(new CreateDB(collectionName, "<database><records></records></database>"))
      //add database/records
      //end transaction
    }
    catch {
      case e: BaseXException => {
        //should do some logging here, bring in slog functionality?
        throw e
      }
    }
    cs
  }

  def drop(collectionName: String) : Unit =
  {

    val clientSession = open(collectionName)
    try {
      clientSession.execute("DROP DB " + collectionName)
    }
    catch {
      case e: BaseXException => {
      }
    }
    finally {
      clientSession.execute(new Close())
    }
  }

  def insertUpdate( recordType : String )(
    collectionName : String, key : String, value : String
  ) : Unit =
  {
    //race condition on the exists. wrap this in a transaction

    //there may be a more efficient way to achieve this in a single query but the fact that XQUF doesnt return results
    //and that we need special root node insert handling on very first insert made this cleaner
    if ( exists( recordType )( collectionName, key ) ) {
      update( recordType )( collectionName, key, value )
    }
    else {
      insert( recordType )( collectionName, key, value )
    }
  }

  def insert( recordType : String )( collectionName : String, key : String, value : String ) =
  {
    val clientSession = open(collectionName)
    val insertTemplate =
      (
        "insert node %NODE% into "
          + "for $db in collection('%COLLNAME%')//records return $db"
        );

    //    report(
    //      "attempting to insert record into database doc in " + collectionName
    //    )

    val record = toRecord( recordType )( key, value )
    //println( "record : \n" + record )

    val insertQry =
      insertTemplate.replace(
        "%NODE%",
        record
      ).replace(
        "%COLLNAME%",
        collectionName
      )

//    println("insertion query : \n" + insertQry)
    try {
      clientSession.execute(new XQuery(insertQry))
    }
    catch {
      case e: BaseXException => {
        // this is so the very first insert works properly before /database/records node exists
        //can be removed to the create logic with a trans, create, add, close trans

                 e.printStackTrace()
//        val records = toRecords(record)
//        clientSession.execute(new Add("database", records))

        //          report(
        //            "adding database doc to " + collectionName
        //          )
        //          tweetTrace(e)
      }
    }
    finally {
      clientSession.execute(new Close())
    }
  }

  def replaceTemplate( recordType : String ) : String = {
    (
      "let $root := collection('%COLLNAME%')//records "
      + "let $key := %KEY% "
      + "for $rcrd in $root/%RECORDTYPE% "
      + "let $rcrdkey := $rcrd/*[1] "
      + "where deep-equal($key, $rcrd/*[1]) "
      + "return if (exists($rcrd)) "
      + "then replace value of node $rcrd/*[2] "
      //+ "return replace value of node $rcrds[1]/*[2] "
      + "with %VALUE% "
      + "else ()"
    ).replace( "%RECORDTYPE%", recordType )
  }

  def update( recordType : String )(collectionName: String, key: String, value: String) =
  {
    //val clientSession = open(collectionName)

    val replTemplate : String = replaceTemplate( recordType );
    //    report(
    //      "attempting to update record in database doc in " + collectionName
    //    )

    //println( "record : \n" + nodeStr )

    val replaceQry =
      replTemplate.replace(
        "%KEY%",
        key
      ).replace(
        "%VALUE%",
        value
      ).replace(
        "%COLLNAME%",
        collectionName
      )

    //    println("update query : \n" + replaceQry)
    //XQUF do not return results
    execute(collectionName, replaceQry)
  }

  def existsTemplate( recordType : String ) : String = {
    (
      "let $root := collection('%COLLNAME%')//records "
      + "let $key := %KEY% "
      + "for $rcrd in $root/%RECORDTYPE% "
      + "let $rcrdkey := $rcrd/*[1] "
      + "where deep-equal($key, $rcrd/*[1]) "
      + "return (exists($rcrd)) "
    ).replace( "%RECORDTYPE%", recordType )
  }

  //exist by id (one attr among many) will likely not work with deep-equal
  def exists( recordType : String )(collectionName: String, key: String): Boolean =
  {
    //val clientSession = clientSessionFromConfig
    val eTemplate : String = existsTemplate( recordType );

    val existsQry =
      eTemplate.replace(
        "%KEY%",
        key
      ).replace(
        "%COLLNAME%",
        collectionName
      )

    try {
      val results = executeScalar(collectionName, existsQry)
      results match {
        case "" => false
        case _ => true
      }
    }
    catch {
      case e: BaseXException => {
//        println(e)
        false
      }
    }
  }

  def recordDeletionQueryTemplate( recordType : String ) : String = {
    (
      "delete node "
      + "let $key := %RecordKeyConstraints% "
      + "for $rcrd in collection( '%COLLNAME%' )//records/%RECORDTYPE% "
      + "where deep-equal($key, $rcrd/*[1]) "
      + "return $rcrd"
    ).replace( "%RECORDTYPE%", recordType )
  }

  def delete( recordType : String )(collectionName: String, key: String) : Unit =
  {
    val rcrdDelQryTemplate : String = 
      recordDeletionQueryTemplate( recordType )

    val deletionQry =
      rcrdDelQryTemplate.replace(
        "%RecordKeyConstraints%",
        key
      ).replace(
        "%COLLNAME%",
        collectionName
      )

    try {
      val results = executeWithResults(collectionName, List(deletionQry))
    }
    catch {
      case e : BaseXException => {
	// BUGBUG -- lgm : should dump this to log
        e.printStackTrace()
      }
    }
  }

  def count(collectionName: String): Int =
  {
    val countQry = "count(collection('" + collectionName + "')//records/record)"
    val results = try {
          executeScalar(collectionName, countQry)
        } catch {
          case e: BaseXException => ""
        }
    results match {
      case "" => 0
      case _ => results.toInt
    }
  }

  //executes defer exception handling to parent method
  def execute(collectionName: String, query: String): Unit =
  {
    execute(collectionName: String, List(query))
  }

  def execute(collectionName: String, queries: List[String]): Unit =
  {
    val clientSession = open(collectionName)
    for (query <- queries) {
      clientSession.execute(new XQuery(query))
    }
  }

  def executeScalar(collectionName: String, query: String): String =
  {

    val clientSession = open(collectionName)
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {

      clientSession.setOutputStream(srvrRspStrm)

      clientSession.execute(new XQuery(query))
      srvrRspStrm.toString
    }
    catch {
      case e: BaseXException => {
        //caught in code review, make more efficient
        throw e
      }
    }
    finally {
      // Reset output stream
      clientSession.execute(new Close())
      srvrRspStrm.close()
    }
  }

//  def executeWithResults(query: String): List[Elem] =
//  {
//    val clientSession = clientSessionFromConfig
//    val srvrRspStrm = new java.io.ByteArrayOutputStream()
//
//    try {
//      clientSession.setOutputStream(srvrRspStrm)
//      clientSession.execute(new XQuery(query))
//      val results = srvrRspStrm.toString("UTF-8")
//
//      results match {
//        case "" => {
//                Nil
//        }
//        case _ => {
//                XML.loadString(
//                  "<results>" + results + "</results>"
//                ).child.toList.filter(
//                  (x: Node) => x.isInstanceOf[Elem]
//                ).asInstanceOf[List[Elem]]
//        }
//      }
//    }
//    catch {
//      case bxe : BaseXException => {
//      	throw( bxe )
//      }
//    }
//    finally{
//      srvrRspStrm.close
//      clientSession.execute(new Close())
//    }
//  }

  def executeWithResults(  collectionName : String, query : String ) : List[Elem] =
  {
    def getRslts(
      srStrm : java.io.ByteArrayOutputStream
    ) : List[Elem] = {
      val results = srStrm.toString("UTF-8")
      //println( "results: " + results )
      results match {
        case "" => {
                Nil
        }
        case _ => {
                XML.loadString(
                  "<results>" + results + "</results>"
                ).child.toList.filter(
                  (x: Node) => x.isInstanceOf[Elem]
                ).asInstanceOf[List[Elem]]
        }
      }
    }
    val clientSession = open(collectionName)
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {


      clientSession.setOutputStream(srvrRspStrm)
      clientSession.execute(new XQuery(query))
      getRslts( srvrRspStrm )
    }
    catch {
      case bxe : BaseXException => {
	      throw bxe
      }
    }
    finally{
      srvrRspStrm.close
      clientSession.execute(new Close())
    }
  }  

//  def executeWithResults(queries: List[String]): List[Elem] =
//  {
//    queries.flatMap(executeWithResults)
//  }
  def executeWithResults( collectionName : String, queries : List[String] ): List[Elem] =
  {
    queries.flatMap(executeWithResults( collectionName, _ ))
  }
}
