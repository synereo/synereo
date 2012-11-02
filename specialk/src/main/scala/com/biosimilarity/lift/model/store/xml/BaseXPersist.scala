package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store._
import org.basex.server._
import com.biosimilarity.lift.model._
import com.biosimilarity.lift.lib._
import org.basex.core._
import org.basex.core.cmd.Open
import org.basex.core.cmd.Close
import org.basex.core.cmd.CreateDB
import org.basex.core.cmd.XQuery
import org.basex.core.cmd.Add
import scala.collection._
import scala.xml._

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
    try {
      val clientSession = clientSessionFromConfig
      clientSession.execute(new Open(collectionName))
      if (!leaveOpen) {
        clientSession.execute(new Close())
      }
      true
    }
    catch {
      case e: BaseXException => {
        false
      }
    }
  }

  def checkIfDBExistsAndCreateIfNot(collectionName: String, leaveOpen: Boolean): Boolean =
  {    
    try {      
      val clientSession = clientSessionFromConfig
      clientSession.execute(new Open(collectionName))
      if (!leaveOpen) {
        clientSession.execute(new Close())
      }
      true
    }
    catch {
      case bxe : BaseXException => {
	val clientSession = clientSessionFromConfig
        val records = toRecords( "" )	
	try {
	  val cs = create(clientSession, collectionName)
	  try {
	    cs.execute(new Open(collectionName))
	    cs.execute(new Add(records, "database"))
	  }
	  catch {
	    case inrBxe : BaseXException => {
	      inrBxe.printStackTrace
	      clientSession.execute(new Close())
	      false
	    }
	  }
	}
	catch {
	  case subBxe : BaseXException => {
	    subBxe.printStackTrace
	    clientSession.execute(new Close())
	    false
	  }
	}	

	if (!leaveOpen) {
          clientSession.execute(new Close())
	}
	true
      }
      case _ => {
	false
      }
    }    
  }

  def open(collectionName: String): ClientSession =
  {
    open(collectionName, 50, 100)
  }

  //open and create if missing
  def open(collectionName: String, retries: Int, wait: Int): ClientSession =
  {
    var cs = clientSessionFromConfig
    for (i <- 1 to retries) {
      try {
        cs.execute(new Open(collectionName))
        //println("stopping at " + i)
        return cs
      }
      catch {
        case e: BaseXException => {
          //println(e)
          cs = create(cs, collectionName)
        }
        Thread.sleep(wait)
      }
      finally {
      }
    }
    return cs
  }

  def create(cs: ClientSession, collectionName: String): ClientSession =
  {
    try {
      //transaction
      cs.execute(new CreateDB(collectionName))
      //add database/records
      //end transaction
    }
    catch {
      case e: BaseXException => {
        //do nothing
        //println(e)
      }
    }
    cs
  }

  def drop(collectionName: String) : Unit =
  {
    val cs = clientSessionFromConfig
    try {
      cs.execute("DROP DB " + collectionName)
    }
    catch {
      case e: BaseXException => {
      }
    }
    finally {
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
          + "for $db in collection('%COLLNAME%')/records return $db"
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

        //          report(
        //            "insertion query failed " + insertQry
        //          )
        val records = toRecords(record)
        clientSession.execute(new Add(records, "database"))

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
      "let $root := collection('%COLLNAME%')/records "
      + "let $key := %KEY% "
      + "for $rcrd in $root/%RECORDTYPE% "
      + "let $rcrdkey := $rcrd/*[1] "
      + "where deep-equal($rcrdkey, $key) "
      + "return if (exists($rcrd)) "
      + "then replace value of node $rcrd/*[2] "
      //+ "return replace value of node $rcrds[1]/*[2] "
      + "with %VALUE% "
      + "else ()"
    ).replace( "%RECORDTYPE%", recordType )
  }

  def update( recordType : String )(collectionName: String, key: String, value: String) =
  {
    val clientSession = open(collectionName)

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
    execute(replaceQry)
  }

  def existsTemplate( recordType : String ) : String = {
    (
      "let $root := collection('%COLLNAME%')/records "
      + "let $key := %KEY% "
      + "for $rcrd in $root/%RECORDTYPE% "
      + "let $rcrdkey := $rcrd/*[1] "
      + "where deep-equal($rcrdkey, $key) "
      + "return (exists($rcrd)) "
    ).replace( "%RECORDTYPE%", recordType )
  }

  //exist by id (one attr among many) will likely not work with deep-equal
  def exists( recordType : String )(collectionName: String, key: String): Boolean =
  {
    val clientSession = clientSessionFromConfig
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
      val results = executeScalar(existsQry)
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
      + "for $rcrd in collection( '%COLLNAME%' )/records/%RECORDTYPE% "
      + "where deep-equal($rcrd/*[1], $key) "
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
      val results = executeWithResults(List(deletionQry))
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
    val countQry = "count(collection('" + collectionName + "')/records/record)"
    val results = try {
          executeScalar(countQry)
        } catch {
          case e: BaseXException => ""
        }
    results match {
      case "" => 0
      case _ => results.toInt
    }
  }

  //executes defer exception handling to parent method
  def execute(query: String): Unit =
  {
    execute(List(query))
  }

  def execute(queries: List[String]): Unit =
  {
    val clientSession = clientSessionFromConfig
    for (query <- queries) {
      clientSession.execute(new XQuery(query))
    }
  }

  def executeScalar(query: String): String =
  {
    val clientSession = clientSessionFromConfig
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
      srvrRspStrm.close()
    }
  }

  def executeWithResults(query: String): List[Elem] =
  {    
    val clientSession = clientSessionFromConfig    
    val srvrRspStrm = new java.io.ByteArrayOutputStream()

    try {      
      clientSession.setOutputStream(srvrRspStrm)
      clientSession.execute(new XQuery(query))
      val results = srvrRspStrm.toString("UTF-8")
      srvrRspStrm.close

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
    catch {
      case bxe : BaseXException => {
	srvrRspStrm.close
	clientSession.execute(new Close())
	throw( bxe )
      }
    }    
  }

  def executeWithResults(  collectionName : String, query : String ) : List[Elem] =
  {
    def getRslts(
      srStrm : java.io.ByteArrayOutputStream
    ) : List[Elem] = {
      val results = srStrm.toString("UTF-8")
      //println( "results: " + results )
      srStrm.close

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

    val clientSession = clientSessionFromConfig    
    val srvrRspStrm = new java.io.ByteArrayOutputStream()

    try {      
      clientSession.execute(new Open(collectionName))
      clientSession.setOutputStream(srvrRspStrm)
      clientSession.execute(new XQuery(query))
      getRslts( srvrRspStrm )
    }
    catch {
      case bxe : BaseXException => {
	println( "warning: " + bxe.getMessage )
	srvrRspStrm.close
	clientSession.execute(new Close())

	val cs = clientSessionFromConfig
	val srStrm = new java.io.ByteArrayOutputStream()
	
	cs.execute(new Open(collectionName))
	cs.setOutputStream(srStrm)

	cs.execute(new XQuery(query))
	getRslts( srStrm )
      }
    }    
  }  

  def executeWithResults(queries: List[String]): List[Elem] =
  {
    queries.flatMap(executeWithResults)
  }
  def executeWithResults( collectionName : String, queries : List[String] ): List[Elem] =
  {
    queries.flatMap(executeWithResults( collectionName, _ ))
  }
}
