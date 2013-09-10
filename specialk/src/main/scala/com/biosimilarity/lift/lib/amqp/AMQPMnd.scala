// -*- mode: Scala;-*- 
// Filename:    AMQPMnd.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jul 12 01:50:41 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import amqp.RabbitFactory
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib.monad._

import net.liftweb.amqp._

import scala.util.continuations._

import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._
import scala.collection.mutable.HashMap

import com.rabbitmq.client.{ Channel => RabbitChan, _}

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.net.URI
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.PrintWriter
import java.io.StringWriter
import java.util.Timer
import java.util.TimerTask

trait AMQPBrokerScope[T] extends Serializable {
  self : MonadicDispatcherScope[T] =>
    def factory : ConnectionFactory  
  /* java.lang.String contentType */
  /* java.lang.String contentEncoding */
  /* java.util.Map<java.lang.String,java.lang.Object> headers */
  /* java.lang.Integer deliveryMode */
  /* java.lang.Integer priority */
  /* java.lang.String correlationId */
  /* java.lang.String replyTo */
  /* java.lang.String expiration */
  /* java.lang.String messageId */
  /* java.util.Date timestamp */
  /* java.lang.String type */
  /* java.lang.String userId */
  /* java.lang.String appId */
  /* java.lang.String clusterId */
  def properties : Option[AMQP.BasicProperties] = None

  class StdMonadicAMQPSndrRcvr[A](
    override val host : String,
    override val port : Int
  ) extends StdMonadicAMQPDispatcher[A]( host, port )
  with Serializable {
    def sender [A] ( 
      host : String,
      port : Int,
      exchange : String,
      routingKey : String
    ) : Generator[Unit, A, Unit] = {      
      val conn = RabbitFactory.getConnection(factory, host, port)
      val channel = conn.createChannel()
      val qname = ( exchange + "_queue" )
	channel.exchangeDeclare( exchange, "direct" )
      channel.queueDeclare( qname, true, false, false, null );
      channel.queueBind( qname, exchange, routingKey )
	  
      Generator {
	k : ( Unit => A @suspendable ) => {
	  spawn {
	    val bytes = new ByteArrayOutputStream
	    val store = new ObjectOutputStream(bytes)
	    store.writeObject( k() )
	    store.close
	    //println( "calling basicPublish on " + channel )
	    channel.basicPublish(
	      exchange,
	      routingKey,
	      properties.getOrElse( null ),
	      bytes.toByteArray
	    )	    
	  }
	}
      }
    }
    
    def senderChannel [A] ( 
      host : String,
      port : Int,      
      exchange : String,
      routingKey : String
    ) : Generator[Channel, Unit, Unit] = {
      Generator {
	k : ( Channel => Unit @suspendable ) => {
	  val conn = RabbitFactory.getConnection(factory, host, port)
	  val channel = conn.createChannel()
	  k( channel )
	}
      }
    }
  }  

  override type MDS[A] = StdMonadicAMQPSndrRcvr[A]
  override def protoMDS[A] =
    new StdMonadicAMQPSndrRcvr[A]( "", -1 )

  trait AMQPAbstractQueue[T] extends Serializable {
    def exchange : String
    def routingKey : String
    
    def dispatcher : theMDS.Generator[T,Unit,Unit]
    def sender : theMDS.Generator[Unit,T,Unit]

    def !( msg : T ) : Unit = {
      //println( "sending message : " + msg )
      reset { for( _ <- sender ) { msg } }
    }

    def !!( msg : T ) : Unit @suspendable = {
      //println( "sending message : " + msg )
      for( _ <- sender ) { msg }
    }

    override def equals( o : Any ) : Boolean = {
      o match {
	case that : AMQPAbstractQueue[T] => {
	  (
	    exchange.equals( that.exchange )
	    && routingKey.equals( that.routingKey )
	    && dispatcher.equals( that.dispatcher )
	    && sender.equals( that.sender )
	  )
	}
	case _ => false
      }
    }
    
    override def hashCode( ) : Int = {
      (	
	( 37 * exchange.hashCode )
	+ ( 37 * routingKey.hashCode )
	+ ( 37 * dispatcher.hashCode )
	+ ( 37 * sender.hashCode )
      )
    }
  }

  trait AMQPQueueFunctor[W,T,AQT[T] <: AMQPAbstractQueue[T]] extends Serializable {
    def makeQueue [S] (
      exchange : String,
      routingKey : String,
      dispatcher : theMDS.Generator[S,Unit,Unit],
      sender : theMDS.Generator[Unit,S,Unit]
    ) : AQT[S]

    def map[W,T]( aQueue : AQT[T], w2T : W => T, t2W : T => W ) : AQT[W] = {
      val aDspr =
	aQueue.dispatcher.mapSrc[W](
	  t2W
	).asInstanceOf[theMDS.Generator[W,Unit,Unit]]
      val aSndr =
	aQueue.sender.mapTrgt[W](
	  w2T
	).asInstanceOf[theMDS.Generator[Unit,W,Unit]]

      makeQueue[W]( aQueue.exchange, aQueue.routingKey,	aDspr, aSndr )
    }
  }

  trait AMQPQueueXForm[W,T]
  extends AMQPAbstractQueue[T] {
    def w2T : W => T
    def t2W : T => W

    @transient var _dispatcher : Option[theMDS.Generator[T,Unit,Unit]] = None
    @transient var _sender : Option[theMDS.Generator[Unit,T,Unit]] = None

    def dispatcherW : theMDS.Generator[W,Unit,Unit]
    def senderW : theMDS.Generator[Unit,W,Unit]

    override def dispatcher : theMDS.Generator[T,Unit,Unit] = {
      _dispatcher match {
	case Some( d ) => d
	case _ => {
	  // BUGBUG -- lgm : the coercion is relatively safe -- we
	  // know that Generator is the only concrete instance of
	  // Generable in play in this codebase at this time
	  val d =
	    dispatcherW.mapSrc(
	      w2T( _ )
	    ).asInstanceOf[theMDS.Generator[T,Unit,Unit]]
	  _dispatcher = Some( d )
	  d
	}
      }
    }
    
    override def sender : theMDS.Generator[Unit,T,Unit] = {
      _sender match {
	case Some( s ) => s
	case _ => {
	  // BUGBUG -- lgm : the coercion is relatively safe -- we
	  // know that Generator is the only concrete instance of
	  // Generable in play in this codebase at this time
	  val s =
	    senderW.mapTrgt(
	      t2W( _ )
	    ).asInstanceOf[theMDS.Generator[Unit,T,Unit]]
	  _sender = Some( s )
	  s
	}
      }
    }    
  }

  class AMQPQueue[A]( 
    override val exchange : String,
    override val routingKey : String,
    @transient override val dispatcher : theMDS.Generator[A,Unit,Unit],
    @transient override val sender : theMDS.Generator[Unit,A,Unit]
  ) extends AMQPAbstractQueue[A]

  object AMQPQueue {
    def apply [A] (
      exchange : String,
      routingKey : String,
      dispatcher : theMDS.Generator[A,Unit,Unit],
      sender : theMDS.Generator[Unit,A,Unit]
    ) : AMQPQueue[A] = {
      new AMQPQueue( exchange, routingKey, dispatcher, sender )
    }
    def unapply [A] (
      amqpQ : AMQPQueue[A]
    ) : Option[( String, String, theMDS.Generator[A,Unit,Unit],theMDS.Generator[Unit,A,Unit] )] = {
      Some(
	(
	  amqpQ.exchange,
	  amqpQ.routingKey,
	  amqpQ.dispatcher,
	  amqpQ.sender
	)
      )
    }
  }

  trait AMQPQueuePreShivMQT[A, QT[Msg] <: AMQPAbstractQueue[Msg]]
  extends ThreadPoolRunnersX
  //extends FJTaskRunnersX
  with BMonad[QT]
  with MonadPlus[QT]
  with MonadFilter[QT] 
  with Serializable {

    def exchange : String
    def routingKey : String        

    override def unit [S] ( s : S ) : QT[S] = {
      val rslt = zero[S]
      spawn {
	reset { for( e <- rslt.sender ) { s } }
      }
      rslt
    }
    override def bind [S,T] (
      amqpS : QT[S],
      f : S => QT[T]
    ) : QT[T] = {
      val acc = zero[T]
      spawn {
	reset {
	  for( s <- amqpS.dispatcher ) {
	    val amqpT = f( s )
	    spawn {
	      reset {
		for( t <- amqpT.dispatcher ) {
		  for( e <- acc.sender ) { t }
		}
	      }
	    }		
	  }
	}
      }
      acc
    }
    override def plus [A] (
      amqpA1 : QT[A],
      amqpA2 : QT[A]
    ) : QT[A] = {
      val amqpA12 = zero[A]
      spawn {
	reset {
	  for( a1 <- amqpA1.dispatcher ) {
	    for( e <- amqpA12.sender ) { a1 }
	  }
	}
      }
      spawn {
	reset {
	  for( a2 <- amqpA2.dispatcher ) {
	    for( e <- amqpA12.sender ) { a2 }
	  }
	}
      }
      amqpA12
    }
    override def mfilter [S] (
      amqpS : QT[S],
      pred : S => Boolean
    ) : QT[S] = {
      val rslt = zero[S]
      spawn {
	reset {
	  for( s <- amqpS.dispatcher ) {
	    if ( pred( s ) ) {
	      for( e <- rslt.sender ) { s }
	    }
	  }
	}
      }
      rslt
    }    

    override def equals( o : Any ) : Boolean = {
      o match {
	case that : AMQPQueuePreShivMQT[A,QT] => {
	  (
	    exchange.equals( that.exchange )
	    && routingKey.equals( that.routingKey )
	  )
	}
	case _ => false
      }
    }
    
    override def hashCode( ) : Int = {
      (	
	( 37 * exchange.hashCode )
	+ ( 37 * routingKey.hashCode )
      )
    }
  }

  trait AMQPQueueMQT[A,QT[Msg] <: AMQPAbstractQueue[Msg]]
  extends AMQPQueuePreShivMQT[A,QT]
  with ForNotationShiv[QT,A] 
  with ForNotationApplyShiv[QT,A] {

    def exchange : String
    def routingKey : String

    override type ForNotationTrampoline[A] = QCell[A]
    class QCell[A](
      queue : QT[A]
    ) extends SCell( queue ) {
      override def foreach ( f : A => Unit ) : Unit = {
	reset { for( msg <- queue.dispatcher ) { f( msg ) } } ;
	()
      }
    }

    override def apply [S] (
      queue : QT[S]
    ) : ForNotationTrampoline[S] = {
      new QCell[S]( queue )
    }    
  }

  class AMQPQueueM[A](
    val host : String,
    val port : Int,
    override val exchange : String,
    override val routingKey : String
  ) extends AMQPQueueMQT[A,AMQPQueue] {            
    override def zero [A] : AMQPQueue[A] = {
      AMQPQueue[A](
	exchange,
	routingKey,
	theMDS.serve[A]( factory, host, port, exchange ),
	theMDS.sender[A]( host, port, exchange, routingKey )
      )
    }    
    override def equals( o : Any ) : Boolean = {
      o match {
	case that : AMQPQueueM[A] => {
	  (
	    host.equals( that.host )
	    && port.equals( that.port )
	    && exchange.equals( that.exchange )
	    && routingKey.equals( that.routingKey )
	  )
	}
	case _ => false
      }
    }
    
    override def hashCode( ) : Int = {
      (
	( 37 * host.hashCode )
	+ ( 37 * port.hashCode )
	+ ( 37 * exchange.hashCode )
	+ ( 37 * routingKey.hashCode )
      )
    }
  }

  object AMQPQueueM {
    def apply [A] (
      host : String,
      port : Int,
      exchange : String,
      routingKey : String
    ) : AMQPQueueM[A] = {
      new AMQPQueueM[A] ( host, port, exchange, routingKey )
    }
    def unapply [A] (
      amqpQM : AMQPQueueM[A]
    ) : Option[( String, Int, String, String )] = {
      Some(
	( amqpQM.host, amqpQM.port, amqpQM.exchange, amqpQM.routingKey )
      )
    }
  }

  case class AMQPQueueHostExchangeM[A] (
    override val host : String,
    override val exchange : String
  ) extends AMQPQueueM[A](
    host,
    AMQPDefaults.defaultPort,
    exchange,
    "routeroute"
  )

  case class AMQPQueueHostURIM[A] (
    uri : URI
  ) extends AMQPQueueM[A](
    uri.getHost,
    {
      uri.getPort match {
	case -1 => AMQPDefaults.defaultPort
	case port => port
      }
    },
    {
      val spath = uri.getPath.split( "/" )
      spath.length match {
      case 1 => AMQPDefaults.defaultExchange
      case 0 => AMQPDefaults.defaultExchange
      case _ => spath( 1 )
      }
    },
    {
      val qmap = new HashMap[String,String]( )
      uri.getQuery.split( "," ).map(
	( x ) => {
	  val kv = x.split( "=" )
	  qmap += ( ( kv( 0 ), kv( 1 ) ) )
	}
      )
      qmap.get( "routingKey" ) match {
	case Some( rk ) => rk
	case _ => AMQPDefaults.defaultRoutingKey
      }
    }
  )

  case class AMQPQueueHostMonikerM[A] (
    moniker : Moniker
  ) extends AMQPQueueM[A](
    moniker.getHost,
    {
      moniker.getPort match {
	case -1 => AMQPDefaults.defaultPort
	case port => port
      }
    },
    {
      val spath = moniker.getPath.split( "/" )
      spath.length match {
      case 1 => AMQPDefaults.defaultExchange
      case 0 => AMQPDefaults.defaultExchange
      case _ => spath( 1 )
      }
    },
    {
      val qmap = new HashMap[String,String]( )
      moniker.getQuery.split( "," ).map(
	( x ) => {
	  val kv = x.split( "=" )
	  qmap += ( ( kv( 0 ), kv( 1 ) ) )
	}
      )
      qmap.get( "routingKey" ) match {
	case Some( rk ) => rk
	case _ => AMQPDefaults.defaultRoutingKey
      }
    }
  )

  def apply [A] (
    m : AMQPQueueM[A],
    queue : AMQPQueue[A]
  ) : theMDS.Generator[A,Unit,Unit] = {
    theMDS.Generator {
      k : ( A => Unit @suspendable ) => {	
	for( msg <- queue.dispatcher ) {
	  import m._
	  k( msg )
	}
      }
    }
  }
}

class AMQPScope [T] (
  @transient override val factory : ConnectionFactory
) extends AMQPBrokerScope[T]
with MonadicDispatcherScope[T] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : AMQPScope[T] => {
	factory.equals( that.factory )
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    37 * factory.hashCode
  }
}

object AMQPScope {
  def apply [T] (
    factory : ConnectionFactory
  ) : AMQPScope[T] = {
    new AMQPScope[T] ( factory )
  }
  def unapply [T] (
    amqpScope : AMQPScope[T]
  ) : Option[( ConnectionFactory )] = {
    Some( ( amqpScope.factory ) )
  }
}

class AMQPNodeJSScope (
  @transient override val factory : ConnectionFactory
) extends AMQPScope[String]( factory ) {
  @transient override val theMDS : MDS[String] = protoMDSNode
  @transient val theNodeMDS : StdMonadicAMQPNodeJSSndrRcvr = protoNode
  def protoMDSNode : MDS[String] = protoNode
  def protoNode : StdMonadicAMQPNodeJSSndrRcvr =
    new StdMonadicAMQPNodeJSSndrRcvr( "", -1 )
  

  @transient override lazy val properties : Option[AMQP.BasicProperties] = 
    Some( new AMQP.BasicProperties.Builder().contentType( "application/json" ).build() )

  class StdMonadicAMQPNodeJSSndrRcvr(
    override val host : String,
    override val port : Int
  ) extends StdMonadicAMQPSndrRcvr[String]( host, port ) {
    def readJSON ( channel : Channel, exQNameRoot : String ) =
      Generator {
	k: ( String => Unit @suspendable) =>
	  shift {
	    outerk: (Unit => Unit) =>
	      reset {
		
  		for (
		  amqpD <- callbacks( channel, exQNameRoot )
		)	{
  		  val routingKey = amqpD.env.getRoutingKey
		  val contentType = amqpD.props.getContentType
		  val deliveryTag = amqpD.env.getDeliveryTag

		  k( new String( amqpD.body ) )
		  channel.basicAck(deliveryTag, false);
		  
		  // Is this necessary?
		  shift { k : ( Unit => Unit ) => k() }
  		}
  		
  		BasicLogService.blog( "readT returning" )
  		outerk()
	      }
	  }
      }

    def serveJSON (
      factory : ConnectionFactory,
      host : String,
      port : Int,
      exQNameRoot : String
    ) = Generator {
      k : ( String => Unit @suspendable ) =>
	//shift {
	BasicLogService.blog(
	  "The rabbit is running... (with apologies to John Updike)"
	)

	for( channel <- acceptConnections( factory, host, port ) ) {
	  spawn {
	    // Open bracket
	    BasicLogService.blog( "Connected: " + channel )
            val qname = (exQNameRoot + "_queue")
            channel.exchangeDeclare( exQNameRoot, "direct" )
            channel.queueDeclare(qname, true, false, false, null);
            channel.queueBind( qname, exQNameRoot, "routeroute" )

            for ( t <- readJSON ( channel, exQNameRoot ) ) { k( t ) }

            // Close bracket
	  }
	}
      //}
    }

    def senderJSON ( 
      host : String,
      port : Int,
      exchange : String,
      routingKey : String
    ) : Generator[Unit, String, Unit] = {      
      val conn = RabbitFactory.getConnection(factory, host, port)
      val channel = conn.createChannel()
      val qname = ( exchange + "_queue" )
	channel.exchangeDeclare( exchange, "direct" )
      channel.queueDeclare( qname, true, false, false, null );
      channel.queueBind( qname, exchange, routingKey )
	  
      Generator {
	k : ( Unit => String @suspendable ) => {
	  spawn {	   
	    channel.basicPublish(
	      exchange,
	      routingKey,
	      properties.getOrElse( null ),
	      k( ).getBytes
	    )	    
	  }
	}
      }
    }
  }

  class AMQPNodeJSQueueM(
    override val host : String,
    override val exchange : String
  ) extends AMQPQueueHostExchangeM[String]( host, exchange ) {            
    def zeroJSON : AMQPQueue[String] = {
      AMQPQueue[String](
	exchange,
	"routeroute",
	theNodeMDS.serveJSON(
	  factory,
	  host,
	  AMQPDefaults.defaultPort,
	  exchange
	).asInstanceOf[AMQPNodeJSScope.this.theMDS.Generator[String,Unit,Unit]],
	theNodeMDS.senderJSON(
	  host,
	  AMQPDefaults.defaultPort,
	  exchange,
	  "routeroute"
	).asInstanceOf[AMQPNodeJSScope.this.theMDS.Generator[Unit,String,Unit]]
      )
    }
  }  
}

object AMQPNodeJSScope {
  def apply (
    factory : ConnectionFactory
  ) : AMQPNodeJSScope = {
    new AMQPNodeJSScope ( factory )
  }
  def unapply (
    amqpScope : AMQPNodeJSScope
  ) : Option[( ConnectionFactory )] = {
    Some( ( amqpScope.factory ) )
  }
}

trait JSONOverAMQPBrokerScope[T] 
extends MonadicDispatcherScope[T] with AMQPBrokerScope[T] {  
  case class AMQPQueueJSONXForm[T](
    override val exchange : String,
    override val routingKey : String,    
    override val w2T : String => T,
    override val t2W : T => String,
    @transient override val dispatcherW : theMDS.Generator[String,Unit,Unit],
    @transient override val senderW : theMDS.Generator[Unit,String,Unit]
  ) extends AMQPQueueXForm[String,T]

  class JSONOverAMQPQueueXFormM[A](
    val host : String,
    val port : Int,
    override val exchange : String,
    override val routingKey : String
  ) extends AMQPQueueMQT[A,AMQPQueueJSONXForm] {
    override def zero [A] : AMQPQueueJSONXForm[A] = {
      val a2S : A => String = 
	( a : A ) => {
	  new XStream( new JettisonMappedXmlDriver ).toXML( a )
	}
      val s2A : String => A = 
	( s : String ) => {
	  new XStream(
	    new JettisonMappedXmlDriver
	  ).fromXML( s ).asInstanceOf[A]
	}
      AMQPQueueJSONXForm[A](
	exchange,
	routingKey,
	s2A,
	a2S,
	theMDS.serve[String]( factory, host, port, exchange ),
	theMDS.sender[String]( host, port, exchange, routingKey )
      )
    }    
  }  
}

class JSONOverAMQPScope [T] (
  @transient override val factory : ConnectionFactory
) extends AMQPBrokerScope[T]
with MonadicDispatcherScope[T] {
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : JSONOverAMQPScope[T] => {
	factory.equals( that.factory )
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    37 * factory.hashCode
  }
}

object JSONOverAMQPScope {
  def apply [T] (
    factory : ConnectionFactory
  ) : JSONOverAMQPScope[T] = {
    new JSONOverAMQPScope[T] ( factory )
  }
  def unapply [T] (
    amqpScope : JSONOverAMQPScope[T]
  ) : Option[( ConnectionFactory )] = {
    Some( ( amqpScope.factory ) )
  }
}

case class AMQPStdScope[T] (  
) extends AMQPScope[T] (
  AMQPDefaults.defaultConnectionFactory
) 

case class AMQPNodeJSStdScope (  
) extends AMQPNodeJSScope (
  AMQPDefaults.defaultConnectionFactory
) 

case class JSONOverAMQPStdScope[T] (  
) extends JSONOverAMQPScope[T] (
  AMQPDefaults.defaultConnectionFactory
) 

package usage {
  import scala.collection.mutable.HashMap
  import java.util.UUID  

  object AMQPSample extends UUIDOps {
    val defaultSrcHost = "10.0.1.5"
    val defaultTrgtHost = "10.0.1.9"

    val defaultQueueUUID = getUUID
    
    val srcSeed = scala.math.round( scala.math.random * 100 ).toInt
    val trgtSeed = scala.math.round( scala.math.random * 100 ).toInt
        
    def setupAndRunTest(
      parity : Boolean,
      srcHost : String,
      trgtHost : String,
      queueStr : String,
      msgCount : Int
    ) = {
      val srcScope = new AMQPStdScope[Int]()
      val trgtScope = new AMQPStdScope[Int]()
      
      val srcQM =
	new srcScope.AMQPQueueHostExchangeM[Int]( srcHost, queueStr )
      val trgtQM =
	new trgtScope.AMQPQueueHostExchangeM[Int]( trgtHost, queueStr )

      val srcQ = srcQM.zero[Int]
      val trgtQ = trgtQM.zero[Int]

      val msgMap = new HashMap[Int,Int]()

      def loop( count : Int ) : Unit = {
	println( "entering msg loop with count : " + count )
	count match {
	  case 0 => {
	    println( "Nothing to do." )
	  }
	  case i => {
	    if ( i < 0 ) {
	      throw new Exception(
		"Tsk, tsk, play fair, now... please keep msg counts positive!"
	      )
	    }
	    else {
	      println(
		"Waiting for a message on queue : " + queueStr
	      )
	      for( msg <- trgtQM( trgtQ ) ) {
		val mms = msgMap.size
		println(
		  (
		    "received msg number " + mms
		    + " with contents " + msg
		    + " on " + queueStr 
		  )
		)

		msgMap += ( ( (mms + 1), msg ) )

		if ( mms == count - 1 ) {
		  if ( !parity ) {
		    println(
		      "Ha! We have the last word with msg " + mms
		    )
		    srcQ ! mms
		  }
		  println( "All " + count + " messages sent and received." )
		  println( "Conversation summary: " )
		  for( order <- 1 to count ) {
		    val msg = msgMap( order )
		    val prefix = "received " + msg + " "
		    val suffix = 
		      order match {
			case 1 => 1 + "st" + " "
			case 2 => 2 + "nd" + " "
			case 3 => 3 + "rd" + " "
			case _ => order + "th" + " "
		      }
		    println( prefix + suffix + "msg" )
		  }
		  println( "Test successful." )
		}
		else {
		  if ( mms < count ) {
		    println( "replying with msg " + mms )
		    srcQ ! mms
		  }
		  else {
		    println( "received unexpected msg: " + msg )
		  }
		}
	      }
	    }
	  }
	}
      }
      
      if ( parity ) {
	println(
	  "sending initial msg " + srcSeed + " on queue " + queueStr
	)
	srcQ ! srcSeed
      }

      loop( msgCount )
    }
    
  }

/*
Welcome to Scala version 2.9.0.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_26).
Type in expressions to have them evaluated.
Type :help for more information.

scala> import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib._

scala> import com.biosimilarity.lift.lib.usage._
import com.biosimilarity.lift.lib.usage._
import com.biosimilarity.lift.lib.usage._

scala> import scala.util.continuations._
import scala.util.continuations._
import scala.util.continuations._

scala> import AMQPSample._
import AMQPSample._
import AMQPSample._

scala> setupAndRunTest( true, defaultSrcHost, defaultTrgtHost, "queueRS", 10 )
setupAndRunTest( true, defaultSrcHost, defaultTrgtHost, "queueRS", 10 )
sending initial msg 47 on queue queueRS
entering msg loop with count : 10
Waiting for a message on queue : queueRS

scala> received msg number 0 with contents 0 on queueRS
replying with msg 0
received msg number 1 with contents 1 on queueRS
replying with msg 1
received msg number 2 with contents 2 on queueRS
replying with msg 2
received msg number 3 with contents 3 on queueRS
replying with msg 3
received msg number 4 with contents 4 on queueRS
replying with msg 4
received msg number 5 with contents 5 on queueRS
replying with msg 5
received msg number 6 with contents 6 on queueRS
replying with msg 6
received msg number 7 with contents 7 on queueRS
replying with msg 7
received msg number 8 with contents 8 on queueRS
replying with msg 8
received msg number 9 with contents 9 on queueRS
All 10 messages sent and received.
Conversation summary: 
received 0 1st msg
received 1 2nd msg
received 2 3rd msg
received 3 4th msg
received 4 5th msg
received 5 6th msg
received 6 7th msg
received 7 8th msg
received 8 9th msg
received 9 10th msg
Test successful.
:q
:q
*/
}

