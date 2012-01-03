// -*- mode: Scala;-*- 
// Filename:    k.scala 
// Authors:     lgm                                                    
// Creation:    Wed Dec 28 14:52:27 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._
import scala.collection.mutable.HashMap
import scala.util.continuations._
import org.prolog4j._
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import biz.source_code.base64Coder.Base64Coder
//import java.util._
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

/** 
 * Usage sample serializing SomeClass instance 
 */
object ObjectToString {  
  /** Read the object from Base64 string. */
  def fromString( s : String ) : java.lang.Object = {
    val data : Array[Byte] = Base64Coder.decode( s )
    val ois : ObjectInputStream =
      new ObjectInputStream( new ByteArrayInputStream(  data ) )
    val o : java.lang.Object = ois.readObject();
    ois.close()
    o
  }

  /** Write the object to a Base64 string. */
  def toString( o : Serializable ) : String = {
    val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos : ObjectOutputStream = new ObjectOutputStream( baos )
    oos.writeObject( o )
    oos.close()
    new String( Base64Coder.encode( baos.toByteArray() ) )
  }
}

object OK {
  def coral( useXStream : Boolean )(
    kmap : HashMap[String, (String => Unit)],
    ksmap : HashMap[String,String],
    key : String
  ) = {
    reset {
      shift {
	( k : ( String => Unit ) ) => {
	  kmap += ( ( key, k ) )
	  val ks =
	    if ( useXStream ) {
	      new XStream( new JettisonMappedXmlDriver() ).toXML( k )
	    }
	    else {
	      ObjectToString.toString( k.asInstanceOf[Serializable] )
	    }
	  println( "continuation as string: " + ks )
	  ksmap += ( ( key, ks ) )	  
	  k( ks )
	}
      }
      println( "resuming..." )
      println( "searching kmap : " + kmap )
      for(
	fns <- kmap.get( key )
      ) {
	println( "fns is " + fns );
      }
    }
  }
}

trait ResourceTermTypes[Namespace,Var,Tag,Value] extends MonadicGenerators {
  trait Resource extends Serializable
  case class Ground( v : Value ) extends Resource
  case class RMap(
    m : TMapR[Namespace,Var,Tag,Value]
  ) extends Resource
  case class RBound(
    rsrc : Option[Resource], soln : Option[Solution[String]]
  ) extends Resource

  type GetRequest = CnxnCtxtLabel[Namespace,Var,Tag]  

  class TMapR[Namespace,Var,Tag,Value]
  extends HashMap[GetRequest,Resource]  

  case class Continuation(
    ks : List[Option[Resource] => Unit @suspendable]
  ) extends Resource  
}

trait ResourceTermTypeScope[Namespace,Var,Tag,Value] {
  type RTTypes <: ResourceTermTypes[Namespace,Var,Tag,Value]
  def protoTermTypes : RTTypes
  val rTT : RTTypes = protoTermTypes
  implicit def toValue( v : Value ) = rTT.Ground( v )
}

object KO extends ResourceTermTypeScope[String,String,String,String] {
  type RTTypes = ResourceTermTypes[String,String,String,String]
  object TheRTT extends RTTypes with Serializable
  override def protoTermTypes : RTTypes = TheRTT

  def coral( useXStream : Boolean )(
    kmap : HashMap[String, rTT.Resource],
    ksmap : HashMap[String,String],
    key : String
  ) = {
    rTT.Generator[Option[rTT.Resource],Unit,Unit] {
      rk : ( Option[rTT.Resource] => Unit @suspendable ) => {
	shift {
	  ( k : ( Unit => Unit ) ) => {	  
	    val kRsrc = rTT.Continuation( List( rk ) )
	    kmap += ( ( key, kRsrc ) )
	    
	    val ks =
	      if ( useXStream ) {
		new XStream( new JettisonMappedXmlDriver() ).toXML( kRsrc )
	      }
	      else {
		ObjectToString.toString( kRsrc )
	      }
	  
	    println( "continuation as string: " + ks )

	    ksmap += ( ( key, ks ) )	  
	    
	    k( )
	  }
	}
	println( "resuming..." )
	println( "searching kmap : " + kmap )
	for(
	  fns <- kmap.get( key )
	) {
	  println( "fns is " + fns );
	}
      }
    }
  }
}
