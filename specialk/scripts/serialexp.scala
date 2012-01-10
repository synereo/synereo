// -*- mode: Scala;-*- 
// Filename:    serialexp.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jan  9 23:42:33 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import scala.actors._
import Actor._
import scala.collection.mutable.LinkedList

import biz.source_code.base64Coder.Base64Coder

import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

object SerializationService {
  def rsrcFromString( rv : String ) : java.lang.Object = {
    val data : Array[Byte] = Base64Coder.decode( rv )
    val ois : ObjectInputStream =
      new ObjectInputStream( new ByteArrayInputStream(  data ) )
    val o : java.lang.Object = ois.readObject();
    ois.close()
    o
  }
  def rsrcToString( rsrc : java.lang.Object ) : String = {
    val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos : ObjectOutputStream = new ObjectOutputStream( baos )
    oos.writeObject( rsrc.asInstanceOf[Serializable] )
    oos.close()
    new String( Base64Coder.encode( baos.toByteArray() ) )
  }
}

@transient
class Hammest( line : String ) extends Actor {
  override def act() {
    receive {
      case _ => println( line )
      act()
    }
  }
}

trait T1[X,Y,Z,CollXType[_],CollYType[_],CollZType[_]] {
  def xs : CollXType[X]
  def ys : CollYType[Y]
  def zs : CollZType[Z]

  def ham : Actor      

  @transient var _hammer : Option[Actor] = None
  def hammer : Actor = {
    _hammer match {
      case Some( h ) => h
      case _ => {
	val h =
	  new Actor {
	    override def act() {
	      receive {
		case _ => println( "i'm not in character" )
		act()
	      }
	    }
	  }
	_hammer = Some( h )
	h
      }
    }
  }

  def hammest : Hammest
}

trait T1Scope[X,Y,Z,CollXType[_],CollYType[_],CollZType[_]] {
  type T1Type <: T1[X,Y,Z,CollXType,CollYType,CollZType]
  def getT1 : T1Type
  val localT1 : T1Type = getT1  
}

trait T1ScopeRefinement[X,Y,Z] extends T1Scope[X,Y,Z,LinkedList,LinkedList,LinkedList] {  
  abstract class CT1(
    override val xs : LinkedList[X],
    override val ys : LinkedList[Y],
    override val zs : LinkedList[Z],
    @transient override val ham : Actor,
    override val hammest : Hammest
  ) extends T1[X,Y,Z,LinkedList,LinkedList,LinkedList] with Serializable {    
  }

  override type T1Type = CT1

  /*
  object MethodActor extends Actor {
    override def act() {
      receive {
	case _ => println( "i'm not in character" )
	act()
      }
    }
  }
  */

  object localizedT1
	   extends CT1(
	     new LinkedList(),
	     new LinkedList(),
	     new LinkedList(),
	     new Actor {
	       override def act() {
		 receive {
		   case _ => println( "i'm not in character" )
		   act()
		 }
	       }
	     },
	     new Hammest( "what's my line?" )
	   )
  override def getT1 : T1Type = localizedT1
}

object T1ScopeRefinementSpecialization extends T1ScopeRefinement[Int,Int,Int] with Serializable {
  import SerializationService._
  
  /*
  object AnotherMethodActor extends Actor {
    override def act() {
      receive {
	case _ => println( "i'm not in character" )
	act()
      }
    }
  }
  */

  def doSerialExperiment : Unit = {
    println( "serializing : " + localT1 )
    val s1 = rsrcToString( localT1 )
    println( "serialized as : " + s1 )
    println( "deserializing " + s1 )
    val o = rsrcFromString( s1 )
    println( "deserialized as " + o )
  }
  def doSerialVarExperiment : Unit = {
    println( "hammer of localT1: " + localT1.hammer )
    println( "serializing : " + localT1 )
    val s1 = rsrcToString( localT1 )
    println( "serialized as : " + s1 )
    println( "deserializing " + s1 )
    val o = rsrcFromString( s1 )
    println( "deserialized as " + o )
  }
}
