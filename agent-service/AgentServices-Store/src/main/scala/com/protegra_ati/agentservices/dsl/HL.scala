// -*- mode: Scala;-*- 
// Filename:    HL.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 27 11:45:05 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.dsl

import java.util.UUID
import java.net.URI

trait AbstractHL {
  type Label  
  type Cnxn   
  type Substitution 

  trait HLExpr
  trait Query {
    def label : Label
    def cnxns : Seq[Cnxn]
  }
  trait ValueExpr[Value] {
    def value : Value
  }
  trait Modification[Value] extends ValueExpr[Value]
  trait Result[Value] extends ValueExpr[Value]

  case object Bottom extends HLExpr

  case class FeedExpr(
    override val label : Label,
    override val cnxns : Seq[Cnxn]
  ) extends HLExpr with Query

  case class ReadExpr(
    override val label : Label,
    override val cnxns : Seq[Cnxn]
  ) extends HLExpr with Query

  case class FetchExpr(
    override val label : Label,
    override val cnxns : Seq[Cnxn]
  ) extends HLExpr with Query

  case class GetExpr(
    override val label : Label,
    override val cnxns : Seq[Cnxn]
  ) extends HLExpr with Query

  case class ScoreExpr(
    override val label : Label,
    override val cnxns : Seq[Cnxn],
    staff : Either[Seq[Cnxn],Seq[Label]]
  ) extends HLExpr with Query

  case class CancelExpr(
    override val label : Label,
    override val cnxns : Seq[Cnxn]
  ) extends HLExpr with Query

  case class InsertContent[Value](
    override val label : Label,
    override val cnxns : Seq[Cnxn],
    override val value : Value
  ) extends HLExpr with Query with Modification[Value]

  case class InsertContentV[Value](
    override val label : Label,
    override val cnxns : Seq[Cnxn],
    override val value : Value
  ) extends HLExpr with Query with Modification[Value]

  case class PutContent[Value](
    override val label : Label,
    override val cnxns : Seq[Cnxn],
    override val value : Value
  ) extends HLExpr with Query with Modification[Value]

  case class ResultExpr[Value](
    substitution : Substitution,
    matchedLabel : Label,
    override val value : Value
  ) extends HLExpr with Result[Value]

  case class PostedExpr[Value](
    override val value : Value
  ) extends HLExpr with Result[Value]  
}

package usage {
  object ConcreteHL extends AbstractHL {
    type Label = String
    type Cnxn = String
    type Substitution = String
  }
}
