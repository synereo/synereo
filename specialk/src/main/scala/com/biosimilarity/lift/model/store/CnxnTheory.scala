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
  val conjuncts : List[CnxnCtxtLabel[Namespace,Var,Tag]]
) extends CnxnCtxtBranch[Namespace,Var,Tag]( nameSpace, conjuncts )

object CnxnCtxtConjunction {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtConjunction : CnxnCtxtConjunction[Namespace,Var,Tag]
  ) : Option[( Namespace, List[CnxnCtxtLabel[Namespace,Var,Tag]] )] = {
    Some( ( cnxnCtxtConjunction.nameSpace, cnxnCtxtConjunction.conjuncts ) )
  }
}

class CnxnCtxtDisjunction[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val disjuncts : List[CnxnCtxtLabel[Namespace,Var,Tag]]
) extends CnxnCtxtBranch[Namespace,Var,Tag]( nameSpace, disjuncts )

object CnxnCtxtDisjunction {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtDisjunction : CnxnCtxtDisjunction[Namespace,Var,Tag]
  ) : Option[( Namespace, List[CnxnCtxtLabel[Namespace,Var,Tag]] )] = {
    Some( ( cnxnCtxtDisjunction.nameSpace, cnxnCtxtDisjunction.disjuncts ) )
  }
}

class CnxnCtxtRule[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val antecedents : List[CnxnCtxtLabel[Namespace,Var,Tag]],
  val consequent : CnxnCtxtLabel[Namespace,Var,Tag]
) extends CnxnCtxtBranch[Namespace,Var,Tag](
  nameSpace,
  antecedents
) {
  override def self = ( labels ++ List( consequent ) ).map( Right( _ ) )
}

object CnxnCtxtRule {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtRule : CnxnCtxtRule[Namespace,Var,Tag]
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

class CnxnCtxtTheory[Namespace,Var,Tag](  
  override val nameSpace : Namespace,
  val rules : List[CnxnCtxtLabel[Namespace,Var,Tag]],
  val facts : List[CnxnCtxtLabel[Namespace,Var,Tag]],
  val goals : List[CnxnCtxtLabel[Namespace,Var,Tag]]
) extends CnxnCtxtBranch[Namespace,Var,Tag](
  nameSpace,
  ( facts ++ rules )
) {
  override def self = ( labels ++ goals ).map( Right( _ ) )
}

object CnxnCtxtTheory {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtTheory : CnxnCtxtTheory[Namespace,Var,Tag]
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

