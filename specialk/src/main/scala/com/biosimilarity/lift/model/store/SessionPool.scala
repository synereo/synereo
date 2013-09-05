package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib.BasicLogService

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.MapLike
import scala.collection.mutable.MapProxy
import scala.collection.mutable.Map

import org.apache.commons.pool.BasePoolableObjectFactory
import org.apache.commons.pool.impl.GenericObjectPool
import java.util.concurrent.{ConcurrentHashMap, Semaphore}
import java.net.URI

trait SessionURIConversionsT {
  implicit def sessionURIToTuple( sessionURI : URI ) : ( String, Int, String, String ) = {
    val ( host, port ) = ( sessionURI.getHost, sessionURI.getPort );   
    val ( user, pwd ) = {
      sessionURI.getUserInfo match {
	case null => { ( "", "" ) }
	case userInfo => {
	  userInfo.split( ":" ).toList match {
	    case usr :: Nil => { ( usr, "" ) }
	    case usr :: pass :: Nil => { ( usr, pass ) }
	    case _ => throw new Exception( "ill-formed userInfo: " + userInfo )
	  }
	}
      }
    }
    ( host, port, user, pwd )
  }

  def getSessionURIFromTuple( scheme : String )(
    host : String, port : Int, user : String, pwd : String, path : String
  ) : URI = {
    val userNameAndPwdStr =
      user match {
	case "" => ""
	case dbUsr => {
	  pwd match {
	    case "" => dbUsr
	    case dbPwd =>
	      dbUsr + ":" + dbPwd + "@"
	  }
	}
      }
    val hostAndPortStr =
      host match {
	case "" => ""
	case dbHost => {
	  port match {
	    case -1 => dbHost
	    case dbPort => dbHost + ":" + dbPort
	  }
	}
      }
    val pathStr =
      path match {
	case "" => ""
	case dbPath => "/" + path
      }
    new URI(
      (
	scheme + "://"
	+ userNameAndPwdStr
	+ hostAndPortStr
	+ pathStr
      )
    )
  }
}

object SessionURIConversions extends SessionURIConversionsT 

trait SessionMapAbstractionsT[ClientSession <: {def close() : Unit}] {
  import scala.collection.JavaConversions._
  import SessionURIConversions._  

  def MAX_OBJECTS_IN_POOL = 2000
  def MAX_CREATIONS = 65  
  
  @transient
  lazy val semaphore = new Semaphore(MAX_CREATIONS)
  
  abstract class PoolableClientSessionFactory(
    val host : String, val port : Int, val user : String, val pwd : String
  ) extends BasePoolableObjectFactory[ClientSession] {

    def getSession : ClientSession

    override def makeObject() : ClientSession = {
      semaphore.acquire
      try {
        getSession
      }
      finally {
        semaphore.release
      }
    }
    
    override def validateObject(obj: ClientSession) = super.validateObject(obj)
    
    override def activateObject(obj: ClientSession) {
      super.activateObject(obj)
    }

    override def passivateObject(obj: ClientSession) {
      super.passivateObject(obj)
    }
    
    override def destroyObject(obj: ClientSession) {
      if (obj != null) {
        // Try to close the object, doing nothing on failure
        try {
          obj.close
        } catch {
          case t : Throwable => {
            val errors : java.io.StringWriter = new java.io.StringWriter()
            t.printStackTrace( new java.io.PrintWriter( errors ) )
            BasicLogService.tweet( "unhandled exception : " + errors.toString( ) );
            //throw( t )
          }
        }
      }
    }    
  }

  def manufactureClientSessionFactory(
    host : String, port : Int,
    user : String, pwd : String
  ) : PoolableClientSessionFactory 

  object PoolableClientSessionFactory {
    def apply(
      host : String, port : Int,
      user : String, pwd : String
    ) : PoolableClientSessionFactory =
      manufactureClientSessionFactory( host, port, user, pwd )
    def apply( sessionURI : URI ) : PoolableClientSessionFactory = {
      val ( host, port, user, pwd ) = sessionURIToTuple( sessionURI )
      manufactureClientSessionFactory( host, port, user, pwd )
    }
    def unapply(
      pcsf : PoolableClientSessionFactory
    ) : Option[( String, Int, String, String )] = {
      Some( ( pcsf.host, pcsf.port, pcsf.user, pcsf.pwd ) )
    }
  }

  trait CHMap[PoolT] extends Map[URI,PoolT] with MapLike[URI,PoolT,CHMap[PoolT]] 
  {
    @transient
    lazy val self : ConcurrentHashMap[URI,PoolT] =
      new ConcurrentHashMap[URI,PoolT]()
    override def empty : CHMap[PoolT] = new CHMap[PoolT] { }
    override def get( uri : URI ) : Option[PoolT] = {
      self.get( uri ) match {
	case null => {
	  val ( host, port, user, pwd ) = sessionURIToTuple( uri )
	  val newPool : PoolT =
	    new GenericObjectPool[ClientSession](
	      PoolableClientSessionFactory(host, port, user, pwd),
	      MAX_OBJECTS_IN_POOL,
	      GenericObjectPool.WHEN_EXHAUSTED_BLOCK,
	      -1, -1
	    ).asInstanceOf[PoolT]
          val tmpPool = self.putIfAbsent( uri, newPool )
	  tmpPool match {
	    case null => Some( newPool )
	    case _ => Some( tmpPool )
	  }
	}
	case pool => Some( pool )
      }
    }
    override def iterator : Iterator[( URI, PoolT )] = {
      val eItr : Iterator[java.util.Map.Entry[URI,PoolT]] = self.entrySet.iterator
      eItr.map( ( e : java.util.Map.Entry[URI,PoolT] ) => ( e.getKey, e.getValue ) )
    }
    override def +=( kv : ( URI, PoolT ) ) = {
      self.put( kv._1, kv._2 )
      this
    }
    override def -=( k : URI ) = {
      self.remove( k )
      this
    }
  }
}

trait URIFromConfigurationT {
  def getSessionURIFromConfiguration : URI
  def defaultSchemeFromConfiguration : String
  def defaultPathFromConfiguration : String
}

abstract class SessionPool[ClientSession <: {def close() : Unit}](
  override val MAX_OBJECTS_IN_POOL : Int,
  override val MAX_CREATIONS : Int,
  implicit val configInfo : URIFromConfigurationT
) extends SessionMapAbstractionsT[ClientSession] with MapProxy[URI,GenericObjectPool[ClientSession]] {
  @transient
  lazy val self : CHMap[GenericObjectPool[ClientSession]] =
    new CHMap[GenericObjectPool[ClientSession]] { }

  implicit def uriFromConfig : URI = configInfo.getSessionURIFromConfiguration
  def scheme : String = configInfo.defaultSchemeFromConfiguration
  def path : String = configInfo.defaultPathFromConfiguration

  def getPool(
    host : String, port : Int, user : String, pwd : String
  ): GenericObjectPool[ClientSession] = {
    self( SessionURIConversions.getSessionURIFromTuple( scheme )( host, port, "", "", path ) )
  }

  def borrowClientSession(
    host : String, port : Int, user : String, pwd : String
  ): ClientSession = {
    getPool( host, port, user, pwd ).borrowObject()
  }

  def returnClientSession(
    cs : ClientSession, host : String, port : Int, user : String, pwd : String
  ) = {
    getPool( host, port, user, pwd ).returnObject( cs )
  }    

  def evictClientSession(
    cs : ClientSession, host : String, port : Int, user : String, pwd : String
  ) = {
    try {
      getPool( host, port, user, pwd ).invalidateObject( cs )
    } catch {
      //case _ =>
      case t : Throwable => {
        val errors : java.io.StringWriter = new java.io.StringWriter()
        t.printStackTrace( new java.io.PrintWriter( errors ) )
        BasicLogService.tweet( "unhandled exception : " + errors.toString( ) );
        //throw( t )
      }
    }
  }
}
