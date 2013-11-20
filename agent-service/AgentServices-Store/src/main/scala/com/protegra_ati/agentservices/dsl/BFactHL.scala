// -*- mode: Scala;-*- 
// Filename:    HL.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 27 11:45:05 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.dsl.bfact

import java.util.UUID
import java.net.URI

trait AbstractBFactHL {
  type Label  
  type Cnxn   
  type Substitution 

  trait BFactHLExpr
  trait Request
  trait Response
  trait BehaviorModification
  trait BehaviorIdentification

  trait Commence {
    def behaviorDefinitionCnxn : Cnxn
    def behaviorDefinitionLabel : Label
    def cnxns : Seq[Cnxn]
    def filters : Seq[Label]
  }
  trait Complete {
    def behaviorInstanceCnxn : Cnxn
    def behaviorInstanceLabel : Label
  }
  trait Register {
    def cnxn : Cnxn
    def label : Label    
    def behavior : String
  }

  case class CommenceInstance(
    override val behaviorDefinitionCnxn : Cnxn,
    override val behaviorDefinitionLabel : Label,
    override val cnxns : Seq[Cnxn],
    override val filters : Seq[Label]
  ) extends Commence with Request with BehaviorModification with BFactHLExpr

  case class CommenceInstances(
    val behaviorDefinitionCnxn : Cnxn,
    val behaviorDefinitionLabels : Seq[Label],
    val cnxnsList : Seq[Seq[Cnxn]],
    val filtersList : Seq[Seq[Label]]
  ) extends Request with BehaviorModification with BFactHLExpr
  
  /* CommenceInstance success response */
  case class InstanceRunning(
    override val behaviorInstanceCnxn : Cnxn,
    override val behaviorInstanceLabel : Label
  ) extends Complete with Response with BehaviorModification with BFactHLExpr

  /* CommenceInstance failure response */
  case class InstanceNotRunning(
    override val behaviorInstanceCnxn : Cnxn,
    override val behaviorInstanceLabel : Label,
    reason : String
  ) extends Complete with Response with BehaviorModification with BFactHLExpr

  case class CompleteInstance(
    override val behaviorInstanceCnxn : Cnxn,
    override val behaviorInstanceLabel : Label
  ) extends Complete with Request with BehaviorModification with BFactHLExpr

  case class InstanceCompleted(
    override val behaviorInstanceCnxn : Cnxn,
    override val behaviorInstanceLabel : Label
  ) extends Complete with Response with BehaviorModification with BFactHLExpr

  case class InstanceIncomplete(
    override val behaviorInstanceCnxn : Cnxn,
    override val behaviorInstanceLabel : Label,
    reason : String
  ) extends Complete with Response with BehaviorModification with BFactHLExpr

  case class MapBehavior(
    override val cnxn : Cnxn,
    override val label : Label,
    override val behavior : String
  ) extends Register with Request with BehaviorIdentification with BFactHLExpr

  case class UnmapBehavior(
    val cnxn : Cnxn,
    val label : Label
  ) extends Request with BehaviorIdentification with BFactHLExpr

  case class WrappedBehaviorIdentifier(
    behaviorURI : String
  ) extends BehaviorIdentification with BFactHLExpr

  case class WrappedBehaviorInstance[InstanceType](
    behaviorInstance : InstanceType
  ) extends BehaviorIdentification with BFactHLExpr

  case object Noop extends BFactHLExpr with Request with Response
}

package usage {
  object ConcreteBFactHL extends AbstractBFactHL {
    type Label = String
    type Cnxn = String
    type Substitution = String
  }
}
