package com.biosimilarity.lift.model.store.xml

import com.biosimilarity.lift.model.store.Persist
import org.basex.server.ClientSession
import com.biosimilarity.lift.model.ApplicationDefaults
import org.basex.core.BaseXException
import org.basex.core.cmd.{List=>_,_}
import scala.xml.{Node, XML, Elem}

trait XMLResultsParser {
  type ParsedResult
  def getRslts(
    srStrm : java.io.ByteArrayOutputStream
  ) : scala.List[ParsedResult]
}
trait ScalaXMLResultsParser extends XMLResultsParser {
  type ParsedResult = Elem  
  override def getRslts( srStrm : java.io.ByteArrayOutputStream ) : scala.List[ParsedResult] = {
    val results = srStrm.toString("UTF-8")
    results match {
      case "" => {
        Nil
      }
      case _ => {
        XML.loadString(
          "<results>" + results + "</results>"
        ).child.toList.filter(
          (x: Node) => x.isInstanceOf[Elem]
        ).asInstanceOf[scala.List[Elem]]
      }
    }
  }
}
trait ScalesXMLResultsParser extends XMLResultsParser {
  import scales.xml.{
      Doc => SXMLDoc, Namespace => SXMLNamespace, Elem => SXMLElem, Text => SXMLText, _
    }
  import scales.utils.{ Tree => SXMLTree, _ }
  import ScalesUtils._
  import ScalesXml._
  import Functions._

  type ParsedResult = 
    SXMLTree[XmlItem,SXMLElem,scales.xml.XCC]

  def getRsltsScalesXML(
    srStrm : java.io.ByteArrayOutputStream
  ) : scala.List[ParsedResult] = {
    val results = srStrm.toString("UTF-8")
    results match {
      case "" => {
        Nil
      }
      case _ => {
        loadXml(
	  new java.io.StringReader(
	    "<results>" + results + "</results>"
	  )
	).rootElem.children.toList.filter( _.isRight ).map( _.getRight )
      }
    }
  }
}

trait BaseXPersist extends Persist[ClientSession,Elem]
with ScalaXMLResultsParser
with Schema
with StdXMLStoreConfiguration
{  
  @transient
  private final val pool = BaseXSessionPool
  
  def clientSessionFromConfig: ClientSession =
  {
    new ClientSession(dbHost, dbPort.toInt, dbUser, dbPwd)
  }

  private def clientSessionFromPool: ClientSession =
  {
    pool.borrowClientSession(dbHost, dbPort.toInt, dbUser, dbPwd)
  }

  /**
   * Any method caller should make sure to return the connection to the pool!
   * @param collectionName
   * @return
   */
  def open(collectionName: String) = {
    val clientSession = clientSessionFromPool
    _checkIfDBExistsAndCreateIfNot(clientSession, collectionName)
    clientSession
  }

  // Note: leaveOpen is being ignored
  def checkIfDBExists(collectionName: String, leaveOpen: Boolean): Boolean =
  {
    val clientSession = clientSessionFromPool
    try {
      val res = _checkIfDBExists(clientSession, collectionName)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _checkIfDBExists(clientSession:ClientSession, collectionName: String): Boolean =
  {
    try {
      clientSession.execute(new Open(collectionName))
      true
    }
    catch {
      case e: BaseXException => {
        false
      }
    }
  }


  // Note: leaveOpen is being ignored
  def checkIfDBExistsAndCreateIfNot(collectionName: String, leaveOpen: Boolean): Boolean =
  {
    val clientSession = clientSessionFromPool
    try {
      val res = _checkIfDBExistsAndCreateIfNot(clientSession, collectionName)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _checkIfDBExistsAndCreateIfNot(clientSession: ClientSession, collectionName: String): Boolean =
  {
    try {
      val exists = _checkIfDBExists(clientSession, collectionName)

      if (!exists) {
        createDb(clientSession, collectionName)
      }
      true
    }
    catch {
      case bxe: BaseXException => {
        bxe.printStackTrace
        false
      }
      case e => {
        e.printStackTrace
        throw e
      }
    }
  }


  private def createDb(clientSession: ClientSession, collectionName: String) =
  {
    synchronized {
      try {
        clientSession.execute(new Open(collectionName))
      }
      catch {
        case bxe : BaseXException => {
          try {
            clientSession.execute(new CreateDB(collectionName, emptyDocument()))
          }
          catch {
            case e => {
              e.printStackTrace
              throw e
            }
          }
        }
      }
    }
  }


  def insertUpdate( recordType : String )(
    collectionName : String, key : String, value : String
    ) : Unit =
  {
    //race condition on the exists. wrap this in a transaction
    val clientSession = open(collectionName)

    try {
      //there may be a more efficient way to achieve this in a single query but the fact that XQUF doesnt return results
      //and that we need special root node insert handling on very first insert made this cleaner
      var res:Unit = null
      if ( _exists( recordType, clientSession )( collectionName, key ) ) {
        res = _update( recordType, clientSession )(  collectionName, key, value )
      }
      else {
        res = _insert( recordType, clientSession )( collectionName, key, value )
      }

      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }



  def exists( recordType : String )(collectionName: String, key: String): Boolean =
  {
    val clientSession = open(collectionName)
    try {
      val res = _exists(recordType, clientSession)(collectionName, key)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _exists( recordType : String, clientSession:ClientSession )(collectionName: String, key: String): Boolean =
  {
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
      val results = _executeScalar(clientSession, collectionName, existsQry)
      results match {
        case "" => false
        case _ => true
      }
    }
    catch {
      case e: BaseXException => {
        false
      }
    }
  }


  def update( recordType : String )(collectionName: String, key: String, value: String) =
  {
    val clientSession = open(collectionName)
    try {
      val res = _update(recordType, clientSession)(collectionName, key, value)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _update( recordType : String, clientSession: ClientSession )(collectionName: String, key: String, value: String) =
  {
    val replTemplate : String = replaceTemplate( recordType );

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

    _execute(clientSession, collectionName, replaceQry)
  }


  def insert( recordType : String )( collectionName : String, key : String, value : String ) =
  {
    val clientSession = open(collectionName)

    try {
      val res = _insert(recordType, clientSession)(collectionName, key, value)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _insert( recordType : String, clientSession: ClientSession )( collectionName : String, key : String, value : String ) =
  {
    val insTemplate : String = insertTemplate()
    val record = toRecord( recordType )( key, value )

    val insertQry =
      insTemplate.replace(
        "%NODE%",
        record
      ).replace(
        "%COLLNAME%",
        collectionName
      )

    try {
      clientSession.execute(new XQuery(insertQry))
    }
    catch {
      case e: BaseXException => {
        e.printStackTrace
      }
    }
  }


  def delete( recordType : String )(collectionName: String, key: String) : Unit =
  {
    val rcrdDelQryTemplate : String = recordDeletionQueryTemplate( recordType )

    val deletionQry =
      rcrdDelQryTemplate.replace(
        "%RecordKeyConstraints%",
        key
      ).replace(
        "%COLLNAME%",
        collectionName
      )

    try {
      executeWithResults(collectionName, List(deletionQry))
    }
    catch {
      case e : BaseXException => {
        // BUGBUG -- lgm : should dump this to log
        e.printStackTrace
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


  def drop(collectionName: String) = {
    // Note: we do not need to check if the DB exists to drop it
    val clientSession = clientSessionFromPool
    try {
      val res = clientSession.execute(new DropDB(collectionName))
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }


  //executes defer exception handling to parent method
  def execute(collectionName: String, query: String): Unit =
  {
    execute(collectionName: String, List(query))
  }

  //executes defer exception handling to parent method
  private def _execute(clientSession: ClientSession, collectionName: String, query: String): Unit =
  {
    _execute(clientSession, collectionName: String, List(query))
  }


  def execute(collectionName: String, queries: List[String]): Unit =
  {
    val clientSession = open(collectionName)
    try {
      val res = _execute(clientSession, collectionName, queries)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _execute(clientSession: ClientSession, collectionName: String, queries: List[String]): Unit =
  {
    for (query <- queries) {
      clientSession.execute(new XQuery(query))
    }
  }


  def executeScalar(collectionName: String, query: String): String =
  {
    val clientSession = open(collectionName)
    try {
      val res = _executeScalar(clientSession, collectionName, query)
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
  }

  private def _executeScalar(clientSession: ClientSession, collectionName: String, query: String): String =
  {
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {
      clientSession.setOutputStream(srvrRspStrm)
      clientSession.execute(new XQuery(query))
      clientSession.setOutputStream(null)
      srvrRspStrm.toString("UTF-8")
    }
    catch {
      case e: BaseXException => {
        //caught in code review, make more efficient
        throw e
      }
    }
    finally {
      srvrRspStrm.close()
    }
  }


  def executeWithResults( collectionName : String, queries : scala.List[String] ): scala.List[Elem] =
  {
    queries.flatMap(executeWithResults( collectionName, _ ))
  }

  def executeWithResults(  collectionName : String, query : String ) : scala.List[Elem] =
  {    
    val clientSession = open(collectionName)
    val srvrRspStrm = new java.io.ByteArrayOutputStream()
    try {
      clientSession.setOutputStream(srvrRspStrm)
      clientSession.execute(new XQuery(query))
      val res = getRslts( srvrRspStrm )
      pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd )
      res
    }
    catch {
      case bxe:BaseXException => {
        pool.returnClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw bxe
      }
      case e => {
        pool.evictClientSession(clientSession, dbHost, dbPort.toInt, dbUser, dbPwd)
        throw e
      }
    }
    finally{
      srvrRspStrm.close
    }
  }


  def createTemplate( collectionName: String ) : String = {
    (
      "if (not(db:exists('%COLLNAME%'))) then"
        + "("
        + "db:create('%COLLNAME%', <database>%RECORDS%</database>, '%COLLNAME%.xml')"
        + ")"
        + "else () "
      ).replace("%COLLNAME%", collectionName)
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

  def insertTemplate() : String = {
    (
      "insert node %NODE% into "
        + "for $db in collection('%COLLNAME%')//records return $db"
      )
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
}
