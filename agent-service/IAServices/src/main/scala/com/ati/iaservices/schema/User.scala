// -*- mode: Scala;-*- 
// Filename:    User.scala 
// Authors:     lgm                                                    
// Creation:    Wed Apr 24 09:30:47 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.ati.iaservices.schema

import scala.collection.SeqProxy

import java.net.URI
import java.util.UUID

trait HumanEngagement {
  type Duration
  type Language

  trait GenericCnxn[S,L,T] {
    def src() : S
    def label() : L
    def trgt() : T
  }

  object GenericCnxn {
    def unapply [S,L,T] (
      gc : GenericCnxn[S,L,T]
    ) : Option[( S, L, T )] = {
      Some( ( gc.src, gc.label, gc.trgt ) )
    }
  }

  class Cnxn[S,L,T](
    override val src : S,
    override val label : L,
    override val trgt : T
  ) extends GenericCnxn[S,L,T]

  case class UserAgentCnxn[Namespace,Tag,Value](
    override val src : URI,
    override val label : CnxnCtxtLabel[Namespace,Tag,Value],
    override val trgt : URI
  ) extends Cnxn[URI,CnxnCtxtLabel[Namespace,Tag,Value],URI]

  trait UserModelElement
  trait UserAttribute

  case class Name(
    override val self : Seq[String]
  ) extends SeqProxy[String] with UserAttribute {
    def firstName() : String = this( 0 )
    def lastName() : String = this.last
  }

  case class Email(
    address : String
  ) extends UserAttribute

  case class Pwd(
    word : String
  ) extends UserAttribute
  
  case class About(
    work : Work,
    education : Education,
    contact : Contact
  ) extends UserModelElement
  
  case class WorkHistory(
    override val self : Seq[WorkEngagement]
  ) extends SeqProxy[Engagement] with UserModelElement
  
  case class Education(
    override val self : Seq[StudyEngagement]
  ) extends SeqProxy[Engagement] with UserModelElement
  
  case class Contact(
    override val self : Seq[URI]
  ) extends SeqProxy[URI] with UserAttribute
  
  case class Engagement(
    duration : Duration,
    description : String,
    role : String
  ) extends UserAgentCnxn with UserModelElement

  case class User(
    name : Name,
    userName : String,
    email : Email,
    pwd : Pwd, 
    language : Language,
    about : About
  ) extends UserModelElement
}

package usage {
  object ConcreteHumanEngagement extends HumanEngagement {
    type Duration = ( String, String );
    type Language = String
  }
}
