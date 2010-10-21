// -*- mode: Scala;-*- 
// Filename:    CnxnTheory.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 19 10:17:41 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.zipper._
import scala.collection.SeqProxy
import java.net.URI

class CnxnCtxtConjunction[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val conjuncts : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
) extends CnxnCtxtBranch[Namespace,Var,Tag]( nameSpace, conjuncts )

object CnxnCtxtConjunction {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtConjunction : CnxnCtxtConjunction[Namespace,Var,Tag]
  ) : Option[
        (
	  Namespace,
	  List[CnxnCtxtLabel[Namespace,Var,Tag]]
	)
      ] = {
    Some( ( cnxnCtxtConjunction.nameSpace, cnxnCtxtConjunction.conjuncts ) )
  }
}

class CnxnCtxtDisjunction[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val disjuncts : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
) extends CnxnCtxtBranch[Namespace,Var,Tag]( nameSpace, disjuncts )

object CnxnCtxtDisjunction {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtDisjunction : CnxnCtxtDisjunction[Namespace,Var,Tag]
  ) : Option[
        (
	  Namespace,
	  List[CnxnCtxtLabel[Namespace,Var,Tag]]
	)
      ] = {
    Some( ( cnxnCtxtDisjunction.nameSpace, cnxnCtxtDisjunction.disjuncts ) )
  }
}

class CnxnRule[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val antecedents : List[CnxnCtxtLabel[Namespace,Var,Tag]],
  val consequent : CnxnCtxtLabel[Namespace,Var,Tag]
) extends AbstractCnxnCtxtBranch[Namespace,Var,Tag]
{
  override def self = ( labels ).map( Right( _ ) )
  override def labels : List[CnxnCtxtLabel[Namespace,Var,Tag]] = {
    antecedents ++ List( consequent )
  }
}

object CnxnRule {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtRule : CnxnRule[Namespace,Var,Tag]
  ) : Option[
	(
	  Namespace,
	  List[CnxnCtxtLabel[Namespace,Var,Tag]],
	  CnxnCtxtLabel[Namespace,Var,Tag]
	)
      ] = {
    Some(
      (
	cnxnCtxtRule.nameSpace,
	cnxnCtxtRule.antecedents,
	cnxnCtxtRule.consequent
      )
    )
  }
}

class CnxnTheory[Namespace,Var,Tag](  
  override val nameSpace : Namespace,
  val rules : List[CnxnCtxtLabel[Namespace,Var,Tag]],
  val facts : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual],
  val goals : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
) extends AbstractCnxnCtxtBranch[Namespace,Var,Tag]
{
  override def self = ( labels ).map( Right( _ ) )
  override def labels : List[CnxnCtxtLabel[Namespace,Var,Tag]] = {
    facts ++ goals
  }
}

object CnxnTheory {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtTheory : CnxnTheory[Namespace,Var,Tag]
  ) : Option[
	(
	  Namespace,
	  List[CnxnCtxtLabel[Namespace,Var,Tag]],
	  List[CnxnCtxtLabel[Namespace,Var,Tag]],
	  List[CnxnCtxtLabel[Namespace,Var,Tag]]
	)
      ] = {
    Some(
      (
	cnxnCtxtTheory.nameSpace,
	cnxnCtxtTheory.rules,
	cnxnCtxtTheory.facts,
	cnxnCtxtTheory.goals
      )
    )
  }
}

