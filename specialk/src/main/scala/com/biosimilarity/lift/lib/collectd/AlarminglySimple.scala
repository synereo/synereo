// -*- mode: Scala;-*- 
// Filename:    AlarminglySimple.scala 
// Authors:     lgm                                                    
// Creation:    Wed Apr 25 13:20:30 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.alarm

import com.biosimilarity.lift.lib.bulk._
import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store.xml._
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib.moniker._
import com.biosimilarity.lift.lib._

//import net.liftweb.json._
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import scala.util.continuations._
import scala.concurrent.{Channel => Chan, _}
import scala.concurrent.cpsops._
import scala.xml._
import scala.xml.XML._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

import biz.source_code.base64Coder.Base64Coder
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import java.util.UUID
import java.net.URI

import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

case class PutVal(
  values : List[Int],
  dstypes : List[String],
  dsnames : List[String],
  time : Double,
  interval : Double,
  host : String,
  plugin : String,
  plugin_instance : String,
  `type` : String,
  type_instance : String
)

package usage {
  object PersistedMonadicKVDBCollectD
     extends PersistedMonadicKVDBNodeScope[String,String,String,String]
     with BulkCollectDImport
     with UUIDOps
     with Serializable
  {
    import SpecialKURIDefaults._
    import identityConversions._

    type MTTypes = MonadicTermTypes[String,String,String,String]
    object TheMTT extends MTTypes with Serializable
    override def protoTermTypes : MTTypes = TheMTT

    type DATypes = DistributedAskTypes
    object TheDAT extends DATypes with Serializable
    override def protoAskTypes : DATypes = TheDAT
    
    override type MsgTypes = DTSMSHRsrc   
    override type RsrcMsgTypes = DTSMSHRsrc   
    
    @transient
    val protoDreqUUID = getUUID()
    @transient
    val protoDrspUUID = getUUID()    

    @transient
    lazy val aLabel = new CnxnCtxtLeaf[String,String,String]( Left( "a" ) )

    object MonadicDRsrcMsgs extends RsrcMsgTypes with Serializable {
      
      @transient
      override def protoDreq : DReq = MDGetRequest( aLabel )
      @transient
      override def protoDrsp : DRsp = MDGetResponse( aLabel, "" )
      @transient
      override def protoJtsreq : JTSReq =
	JustifiedRequest(
	  protoDreqUUID,
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  new URI( "agent", protoDreqUUID.toString, "/invitation", "" ),
	  getUUID(),
	  protoDreq,
	  None
	)
      @transient
      override def protoJtsrsp : JTSRsp = 
	JustifiedResponse(
	  protoDreqUUID,
	  new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	  new URI( "agent", protoDrspUUID.toString, "/invitation", "" ),
	  getUUID(),
	  protoDrsp,
	  None
	)
      override def protoJtsreqorrsp : JTSReqOrRsp =
	Left( protoJtsreq )
    }
    
    override def protoMsgs : MsgTypes = MonadicDRsrcMsgs
    override def protoRsrcMsgs : RsrcMsgTypes = MonadicDRsrcMsgs

    object Being extends PersistenceScope with Serializable {      
      override type EMTypes = ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
      object theEMTypes extends ExcludedMiddleTypes[mTT.GetRequest,mTT.GetRequest,mTT.Resource]
       with Serializable
      {
	case class PrologSubstitution( soln : LinkedHashMap[String,CnxnCtxtLabel[String,String,String]] )
	   extends Function1[mTT.Resource,Option[mTT.Resource]] {
	     override def apply( rsrc : mTT.Resource ) = {
	       Some( mTT.RBoundHM( Some( rsrc ), Some( soln ) ) )
	     }
	   }
	override type Substitution = PrologSubstitution	
      }      

      override def protoEMTypes : EMTypes =
	theEMTypes

      object PersistedKVDBNodeFactory extends PersistedKVDBNodeFactoryT with Serializable {	  
	def mkCache[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI ) : PersistedMonadicKVDB[ReqBody,RspBody] = {
	  new PersistedMonadicKVDB[ReqBody,RspBody]( MURI( here ) ) with Blobify with AMQPMonikerOps {		
	    class StringXMLDBManifest(
	      override val storeUnitStr : String,
	      @transient override val labelToNS : Option[String => String],
	      @transient override val textToVar : Option[String => String],
	      @transient override val textToTag : Option[String => String]
	    )
	    extends XMLDBManifest( database ) {
	      override def valueStorageType : String = {
		throw new Exception( "valueStorageType not overriden in instantiation" )
	      }
	      override def continuationStorageType : String = {
		throw new Exception( "continuationStorageType not overriden in instantiation" )
	      }
	      
	      override def storeUnitStr[Src,Label,Trgt]( cnxn : Cnxn[Src,Label,Trgt] ) : String = {     
		cnxn match {
		  case CCnxn( s, l, t ) => s.toString + l.toString + t.toString
		}	    
	      }	
	      
	      def kvNameSpace : String = "record"
	      def kvKNameSpace : String = "kRecord"
	      
	      def compareNameSpace( ns1 : String, ns2 : String ) : Boolean = {
		ns1.equals( ns2 )
	      }
	      
	      override def asStoreValue(
		rsrc : mTT.Resource
	      ) : CnxnCtxtLeaf[String,String,String] with Factual = {
		tweet(
		  "In asStoreValue on " + this + " for resource: " + rsrc
		)
		val storageDispatch = 
		  rsrc match {
		    case k : mTT.Continuation => {
		      tweet(
			"Resource " + rsrc + " is a continuation"
		      )
		      continuationStorageType
		    }
		    case _ => {
		      tweet(
			"Resource " + rsrc + " is a value"
		      )
		      valueStorageType
		    }
		  };
		
		tweet(
		  "storageDispatch: " + storageDispatch
		)
		
		val blob =
		  storageDispatch match {
		    case "Base64" => {
		      val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
		      val oos : ObjectOutputStream = new ObjectOutputStream( baos )
		      oos.writeObject( rsrc.asInstanceOf[Serializable] )
		      oos.close()
		      new String( Base64Coder.encode( baos.toByteArray() ) )
		    }
		    case "CnxnCtxtLabel" => {
		      tweet(
			"warning: CnxnCtxtLabel method is using XStream"
		      )
		      toXQSafeJSONBlob( rsrc )		  		  
		    }
		    case "XStream" => {
		      tweet(
			"using XStream method"
		      )
		      
		      toXQSafeJSONBlob( rsrc )
		    }
		    case _ => {
		      throw new Exception( "unexpected value storage type" )
		    }
		  }
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( blob )
		)
	      }
	      
	      def asCacheValue(
		ccl : CnxnCtxtLabel[String,String,String]
	      ) : String = {
		tweet(
		  "converting to cache value"
		)
		ccl match {
		  case CnxnCtxtBranch(
		    "string",
		    CnxnCtxtLeaf( Left( rv ) ) :: Nil
		  ) => {

		    tweet( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		    tweet( "unpacking rv: " + rv )
		    tweet( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )

		    val unBlob =
		      fromXQSafeJSONBlob( rv )
		    
		    unBlob match {
		      case rsrc : mTT.Resource => {
			getGV( rsrc ).getOrElse( "" )
		      }
		    }
		  }
		  case _ => {
		    //asPatternString( ccl )
		    throw new Exception( "unexpected value form: " + ccl )
		  }
		}
	      }
	      
	      override def asResource(
		key : mTT.GetRequest, // must have the pattern to determine bindings
		value : Elem
	      ) : emT.PlaceInstance = {
		val ttt = ( x : String ) => x
		
		val ptn = asPatternString( key )
		//println( "ptn : " + ptn )		
		
		val oRsrc : Option[emT.PlaceInstance] =
		  for(
		    ltns <- labelToNS;
		    ttv <- textToVar;
		    ccl <- xmlIfier.fromXML( ltns, ttv, ttt )( value )
		  ) yield {
		    ccl match {
		      case CnxnCtxtBranch( ns, k :: v :: Nil ) => {
			val oGvOrK = 
			  (if ( compareNameSpace( ns, kvNameSpace ) ) {	    
			    // BUGBUG -- LGM need to return the Solution
			    // Currently the PersistenceManifest has no access to the
			    // unification machinery	      
			    
			    for ( vCCL <- asCacheValue( ltns, ttv, value ) ) 
			    yield {				    
			      Left[mTT.Resource,mTT.Resource]( mTT.Ground( vCCL ) )
			    }
			  }
			   else {
			     if ( compareNameSpace( ns, kvKNameSpace ) ) {
			       Some( Right[mTT.Resource,mTT.Resource]( asCacheK( v ) ) )
			     }
			     else {
			       throw new Exception( "unexpected namespace : (" + ns + ")" )
			     }
			   });
			
			val cclKey =
			  xmlIfier.fromXML( ltns, ttv, ttt )(
			    xmlIfier.asXML( key )
			  ) match {
			    case Some( cclX ) => cclX
			    case _ => throw new Exception( "xml roundtrip failed " + key )
			  }
			
			matchMap( cclKey, k ) match {
			  case Some( soln ) => {
			    emT.PlaceInstance(
			      k,
			      oGvOrK match {
				case Some( Left( gv ) ) => {
				  Left[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( gv )
				}
				case Some( Right( mTT.Continuation( ks ) ) ) => {
				  Right[mTT.Resource,List[Option[mTT.Resource] => Unit @suspendable]]( ks )
				}
				case _ => {
				  throw new Exception( "excluded middle contract broken: " + oGvOrK )
				}
			      },
			      // BUGBUG -- lgm : why can't the compiler determine
			      // that this cast is not necessary?
			      theEMTypes.PrologSubstitution( soln ).asInstanceOf[emT.Substitution]
			    )
			  }
			  case None => {
			    tweet( "Unexpected matchMap failure: " + cclKey + " " + k )
			    throw new Exception( "matchMap failure " + cclKey + " " + k )
			  }
			}						
		      }
		      case _ => {
			throw new Exception( "unexpected record format : " + value )
		      }
		    }      
		  }
		
		// BUGBUG -- lgm : this is a job for flatMap
		oRsrc match {
		  case Some( pI ) => {
		    pI
		  }
		  case _ => {
		    throw new Exception( "violated excluded middle : " + oRsrc )
		  }
		}
	      }
	      
	    }
	    override def asCacheK(
	      ccl : CnxnCtxtLabel[String,String,String]
	    ) : Option[mTT.Continuation] = {
	      tweet(
		"converting to cache continuation stack" + ccl
	      )
	      ccl match {
		case CnxnCtxtBranch(
		  "string",
		  CnxnCtxtLeaf( Left( rv ) ) :: Nil
		) => {
		  val unBlob =
		    continuationStorageType match {
		      case "CnxnCtxtLabel" => {
			// tweet(
			// 		      "warning: CnxnCtxtLabel method is using XStream"
			// 		    )
			fromXQSafeJSONBlob( rv )
		      }
		      case "XStream" => {
			fromXQSafeJSONBlob( rv )
		      }
		      case "Base64" => {
			val data : Array[Byte] = Base64Coder.decode( rv )
			val ois : ObjectInputStream =
			  new ObjectInputStream( new ByteArrayInputStream(  data ) )
			val o : java.lang.Object = ois.readObject();
			ois.close()
			o
		      }
		    }
		  
		  unBlob match {
		    case k : mTT.Resource => {
		      Some( k.asInstanceOf[mTT.Continuation] )
		    }
		    case _ => {
		      throw new Exception(
			(
			  ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
			  + "ill-formatted continuation stack blob : " + rv
			  + "\n" 
			  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
			  + "\n"
			  + "unBlob : " + unBlob
			  + "\n"
			  + "unBlob type : " + unBlob
			  + "\n"
			  + ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
			)
		      )
		    }
		  }
		}
		case _ => {
		  throw new Exception( "ill-formatted continuation stack leaf: " + ccl )
		}
	      }
	    }
	    
	    override def asCacheK(
	      ltns : String => String,
	      ttv : String => String,
	      value : Elem
	    ) : Option[mTT.Continuation] = {
	      throw new Exception( "shouldn't be calling this version of asCacheK" )
	    }
	    override def persistenceManifest : Option[PersistenceManifest] = {
	      val sid = Some( ( s : String ) => s )
	      val kvdb = this;
	      Some(
		new StringXMLDBManifest( dfStoreUnitStr, sid, sid, sid ) {
		  override def valueStorageType : String = {
		    kvdb.valueStorageType
		  }
		  override def continuationStorageType : String = {
		    kvdb.continuationStorageType
		  }
		}
	      )
	    }
	    def dfStoreUnitStr : String = mnkrExchange( name )
	  }
	}
	def ptToPt[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI, there : URI ) : PersistedMonadicKVDBNode[ReqBody,RspBody] = {
	  val node =
	    PersistedMonadicKVDBNode[ReqBody,RspBody](
	      mkCache( MURI( here ) ),
	      List( MURI( there ) )
	    )
	  spawn { node.dispatchDMsgs() }
	  node
	}
	def loopBack[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse]( here : URI ) : PersistedMonadicKVDBNode[ReqBody,RspBody] = {
	  val exchange = uriExchange( here )
	  val hereNow =
	    new URI(
	      here.getScheme,
	      here.getUserInfo,
	      here.getHost,
	      here.getPort,
	      "/" + exchange + "Local",
	      here.getQuery,
	      here.getFragment
	    )
	  val thereNow =
	    new URI(
	      here.getScheme,
	      here.getUserInfo,
	      here.getHost,
	      here.getPort,
	      "/" + exchange + "Remote",
	      here.getQuery,
	      here.getFragment
	    )	    
	  
	  val node =
	    PersistedMonadicKVDBNode[ReqBody,RspBody](
	      mkCache( MURI( hereNow ) ),
	      List( MURI( thereNow ) )
	    )
	  spawn { node.dispatchDMsgs() }
	  node
	}
      }
    }

    @transient
    lazy val testRunID = getUUID
    @transient
    var runNum = 0

    override def supplyEntries( host : String, queue : String, numOfEntries : Int ) : Unit = {
      // create an AMQP scope
      val collectDAMQPScope = new AMQPStdScope[String]()
      // create an AMQP Queue monad
      val collectDQM =
	new collectDAMQPScope.AMQPQueueHostExchangeM[String](
	  host,
	  queue
	)
      // get an empty queue
      val collectDQ = collectDQM.zero[String]    
      println( "creating entries" )
      for( i <- 1 to numOfEntries ) {
	print( "." )
	val entry = """ { "putval" : { "values":[558815,43649779],"dstypes":["derive","derive"],"dsnames":["rx","tx"],"time":1334349094.633,"interval":10.000,"host":"server-75530.localdomain","plugin":"interface","plugin_instance":"eth0","type":"if_octets","type_instance":"" } } """
	collectDQ ! entry
      }
      println( "\nentries created" )
    }
    
    override def handleEntry( json : JValue, acc : Buffer[Elem] ) : Buffer[Elem] = {
      for(
	JObject( fvs ) <- json \\ "putval" ;
	JArray( valueArray ) <- json \\ "values" ;
	JArray( dstypes ) <- json \\ "dstypes" ;
	JArray( dsnames ) <- json \\ "dsnames" ;
	JDouble( time ) <- json \\ "time" ;
	JDouble( interval ) <- json \\ "interval" ; 
	JString( host ) <- json \\ "host" ; 
	JString( plugin ) <- json \\ "plugin" ;
	JString( plugin_instance ) <- json \\ "plugin_instance" ;
	JString( cdtype ) <- json \\ "type" ;
	JString( type_instance ) <- json \\ "type_instance"
      ) {
	val gvStr =
	  new XStream( new JettisonMappedXmlDriver() ).toXML( mTT.Ground( ( getUUID + "" ) ) )
	
	acc +=
	// <record>
	// 	<putval>
	// 	  <values>{for( JInt( v ) <- valueArray ) yield {<int>{v}</int>}}</values>
	// 	  <dstypes>{for( JString( t ) <- dstypes ) yield {<string>{t}</string>}}</dstypes>
	// 	  <dsnames>{for( JString( n ) <- dsnames ) yield {<string>{n}</string>}}</dsnames>
	//           <time>{time}</time>
	// 	  <interval>{interval}</interval>
	//           <host>{host}</host>
	//           <plugin>{plugin}</plugin>
	//           <plugin_instance>{plugin_instance}</plugin_instance>
	//           <type>{cdtype}</type>
	//           <type_instance>{type_instance}</type_instance>
	// 	</putval>
	// 	<string>{getUUID + ""}</string>
	//       </record>            
	<record>
	  <comBiosimilarityLiftLibAlarmPutVal>
	    <values>{for( JInt( v ) <- valueArray ) yield {<string>{v}</string>}}</values>
	    <dstypes>{for( JString( t ) <- dstypes ) yield {<string>{t}</string>}}</dstypes>
	    <dsnames>{for( JString( n ) <- dsnames ) yield {<string>{n}</string>}}</dsnames>
            <time>{<string>{time}</string>}</time>
	    <interval>{<string>{interval}</string>}</interval>
            <host>{<string>{host}</string>}</host>
            <plugin>{<string>{plugin}</string>}</plugin>
            <plugin_instance>{<string>{plugin_instance}</string>}</plugin_instance>
            <type>{<string>{cdtype}</string>}</type>
            <type_instance>{<string>{type_instance}</string>}</type_instance>
	  </comBiosimilarityLiftLibAlarmPutVal>
	  <string>{gvStr}</string>
	</record>
      }
      acc
    }

    override def readEntries( host : String, queue : String, file : String, dbChunk : Int ) : ListBuffer[String] = {
      // create an AMQP scope
      val collectDAMQPScope = new AMQPStdScope[String]()
    // create an AMQP Queue monad
      val collectDQM =
	new collectDAMQPScope.AMQPQueueHostExchangeM[String](
	  host,
	  queue
	)
      // get an empty queue
      val collectDQ = collectDQM.zero[String]    
      
      val acc = new ListBuffer[Elem]()      
      val fileNames = new ListBuffer[String]()      
      val lock = new Lock()
      
      println( "reading entries" )
      for ( entry <- collectDQM( collectDQ ) ) {
	print( "." )
	handleEntry( parse( entry ), acc )
	lock.acquire
	if ( acc.size > dbChunk ) {
	  runNum += 1
	  val recordsFileName = ( file + testRunID + runNum + ".xml" )	    
	  val db = <records>{acc.toList}</records>
	  
	  println( "\nsaving a chunk of records ( " + dbChunk + " ) to " + recordsFileName )
	  scala.xml.XML.saveFull( recordsFileName, db, "UTF-8", true, null )
	  fileNames += recordsFileName
	  acc.clear
	}
	lock.release	
      }      
      println( "\nentries read" )      
      fileNames
    }

    def loadData() : Unit = {
      supplyEntries( "localhost", "collectDSample", 1000 )
    }
    def importData() : List[String] = {
      val s = new Object {
	def chill() {
	  synchronized {
	    this.wait
	  }
	}
	def go() {
	  synchronized {
	    this.notifyAll()
	  }
	}
      }
      
      val lb = readEntries( "localhost", "collectDSample", "collectDImport", 500 )

      def spin( n : Int ) {
	print( "*" )
	if ( lb.size < 1 ) { 
	  (new Thread {
	    override def run() : Unit = {	      
	      spin( n+1 )
	    }
	  }).start
	  if ( n < 1 ) { s.chill }
	}
	else { s.go }
      }

      spin( 0 )
      lb.toList
    }
  }

  object CollectDPutValUseCase extends Serializable {
    import PersistedMonadicKVDBCollectD._
    import Being._
    import PersistedKVDBNodeFactory._

    import CnxnConversionStringScope._

    def mkPutValQry( putval : PutVal ) : CnxnCtxtLabel[String,String,String] = {
      import CnxnConversionStringScope._
      putval match {
	case cc : ScalaObject with Product with Serializable => {
	  asCnxnCtxtLabel( cc )
	}
	case _ => throw new Exception( "non concrete putval: " + putval )
      }
    }
    
    def mkPutValPtn( putval : PutVal, vars : List[(String,String)] ) : CnxnCtxtLabel[String,String,String] = {      
      putval match {
	case cc : ScalaObject with Product with Serializable => {
	  partialCaseClassDerivative( cc, vars )
	}
	case _ => throw new Exception( "non concrete putval: " + putval )
      }
    }

    /*
    PutVal( li, ls, t, _, host, _, _, _, _ ) -> {
      println( "on " + host + " at " + t " observed: " + li.zip( ls ) )
    }
    
    ( pv1 / List( ( "values", "li" ), ( "dstypes", "ls" ), ( "time", "t" ), ( "host", "host" ) ) ) -> {
      println( "on " + host + " at " + t " observed: " + li.zip( ls ) )
    }
    */

    implicit val retTwist : Boolean = false
    implicit lazy val defaultDataLocation : String =
      //"/collectDImport" + testRunID + runNum
      "/collectDImport" + "0b61b90e-3ef7-4e47-b046-ffa5ac6b0ee08"

    def setup[ReqBody <: PersistedKVDBNodeRequest, RspBody <: PersistedKVDBNodeResponse](
      dataLocation : String,
      localHost : String, localPort : Int,
      remoteHost : String, remotePort : Int
    )(
      implicit returnTwist : Boolean
    ) : Either[Being.PersistedMonadicKVDBNode[ReqBody,RspBody],(Being.PersistedMonadicKVDBNode[ReqBody, RspBody],Being.PersistedMonadicKVDBNode[ReqBody, RspBody])] = {
      val ( localExchange, remoteExchange ) = 
	if ( localHost.equals( remoteHost ) && ( localPort == remotePort ) ) {
	  ( dataLocation, dataLocation + "Remote" )	  
	}
	else {
	  ( dataLocation, dataLocation )	  
	}

      if ( returnTwist ) {
	Right[Being.PersistedMonadicKVDBNode[ReqBody,RspBody],(Being.PersistedMonadicKVDBNode[ReqBody, RspBody],Being.PersistedMonadicKVDBNode[ReqBody, RspBody])](
	  (
	    ptToPt[ReqBody, RspBody](
	      new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	    ),
	    ptToPt[ReqBody, RspBody](	      
	      new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null ),
	      new URI( "agent", null, localHost, localPort, localExchange, null, null )
	    )
	  )
	)
      }
      else {
	Left[Being.PersistedMonadicKVDBNode[ReqBody, RspBody],(Being.PersistedMonadicKVDBNode[ReqBody, RspBody],Being.PersistedMonadicKVDBNode[ReqBody, RspBody])](
	  ptToPt(
	    new URI( "agent", null, localHost, localPort, localExchange, null, null ),
	    new URI( "agent", null, remoteHost, remotePort, remoteExchange, null, null )
	  )
	)
      }
    }
    
    def kvdb( dataLocation : String ) : PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse] = {
      val Right( ( client, server ) ) =
	setup[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse](
	  dataLocation, "localhost", 5672, "localhost", 5672
	)( true )
      client
    }
    case class CCWrapper(
      cc : ScalaObject with Product with Serializable,
      kvdb : PersistedMonadicKVDBNode[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]
    ) {
      def /( vars : List[(String,String)] ) : CnxnCtxtLabel[String,String,String] = {
	partialCaseClassDerivative( cc, vars )
      }
      def ->( ptn : CnxnCtxtLabel[String,String,String] )( body : Unit => Unit ) = {
	reset {
	  for( rsrc <- kvdb.get( ptn ) ) { body() }
	}
      }
    }
    def putval2Ptn( dataSource : String, putval : PutVal ) : CCWrapper = {
      CCWrapper( putval, kvdb( dataSource ) )
    }
    implicit lazy val pvOne =
      PutVal(
	List( 558815, 43649779 ),
	List( "derive", "derive" ),
	List( "rx", "tx" ),
	1334349094.633,
	10.000,
	"server-75530.localdomain",
	"interface",
	"eth0",
	"if_octets",
	""
      )
    def alarm1( dataSource : String, pv : PutVal ) = {      
      ( putval2Ptn( dataSource, pv ) / List( ( "time", "t" ), ( "host", "host" ) ) ) -> {
	println( "Alarmed!" )
      }
    }
  }
}
