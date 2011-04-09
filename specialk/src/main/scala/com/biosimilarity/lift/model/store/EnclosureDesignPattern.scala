// -*- mode: Scala;-*- 
// Filename:    EnclosureDesignPattern.scala 
// Authors:     lgm                                                    
// Creation:    Sat Apr  9 07:45:53 2011 
// Copyright:   Not supplied 
// Description: Executable documentation for parametric enclosure
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

case class Q1( x : Int, y : Int, z : Int )

trait EnclosureOne[T1,T2,T3] {
  case class P1[T1,T2,T3]( t1 : T1, t2 : T2, t3 : T3 )
  def protoP1 : P1[T1,T2,T3]
}

trait EnclosureOneScope[T1,T2,T3] {
  type EO <: EnclosureOne[T1,T2,T3]
  def protoEO : EnclosureOne[T1,T2,T3]
  lazy val eO = protoEO
}

trait EnclosureTwo[T1,T2,T3] {
  case class P2[T1,T2,T3](
    t1 : Option[T1],
    t2 : Option[T2],
    t3 : Option[T3]
  )
  def protoP2 : P2[T1,T2,T3]
}

trait EnclosureTwoScope[T1,T2,T3] {
  type ET <: EnclosureTwo[T1,T2,T3]
  def protoET : EnclosureTwo[T1,T2,T3]
  lazy val eT = protoET
}

trait Trampoline[T1,T2,T3] {
  self : EnclosureOneScope[T1,T2,T3] =>
    trait QtoP[T1,T2,T3] {
      def asP( q : Q1 ) : Option[eO.P1[T1,T2,T3]]
    }
  type QtoPCnv <: QtoP[T1,T2,T3]

  def protoQtoPCnv : QtoPCnv
  lazy val qToP = protoQtoPCnv
}

trait TrampolineScope[T1,T2,T3] {
  type TrampolineTypes <: Trampoline[T1,T2,T3] with EnclosureOneScope[T1,T2,T3]
  def protoTramp : TrampolineTypes
  lazy val tramp = protoTramp
}

object Enclosure
       extends EnclosureOneScope[String,String,String]
       with EnclosureTwoScope[String,String,String]
       with TrampolineScope[String,String,String]
{
  override type ET = EnclosureTwo[String,String,String]
  object theET extends ET {
    override def protoP2 = P2( None, None, None )
  }
  override def protoET = theET

  override type TrampolineTypes =
    Trampoline[String,String,String] with EnclosureOneScope[String,String,String]
  
  object theTramp extends Trampoline[String,String,String]
       with EnclosureOneScope[String,String,String]
  {
    override type EO = EnclosureOne[String,String,String]
    object theEO extends EO {
      override def protoP1 = P1( "", "", "" )
    }
    override def protoEO = theEO

    override type QtoPCnv = QtoP[String,String,String]

    import eO._
    object theQToPCnv extends QtoPCnv
    {
      override def asP( q : Q1 ) =
	Some( P1( q.x.toString, q.y.toString, q.z.toString ) )
    }
     override def protoQtoPCnv = theQToPCnv    
   }

  override def protoTramp = theTramp
  override type EO = tramp.EO
  override def protoEO = tramp.protoEO
}

