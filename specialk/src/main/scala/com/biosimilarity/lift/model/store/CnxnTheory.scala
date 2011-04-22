// -*- mode: Scala;-*- 
// Filename:    CnxnTheory.scala 
// Authors:     lgm                                                    
// Creation:    Tue Oct 19 10:17:41 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib.zipper._
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
  val antecedents : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual],
  val consequent : CnxnCtxtLabel[Namespace,Var,Tag] with Factual
) extends TreeSection[Either[Tag,Var]]( antecedents ++ List( consequent ) )
with AbstractCnxnCtxtBranch[Namespace,Var,Tag]
with Hypothetical
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
  val rules : List[CnxnCtxtLabel[Namespace,Var,Tag] with Hypothetical],
  val facts : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual],
  val goals : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
) extends TreeSection[Either[Tag,Var]]( rules ++ facts ++ goals )
with AbstractCnxnCtxtBranch[Namespace,Var,Tag]
with Theoretical
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

trait CnxnTheoryConversions[Namespace,Var,Tag]
extends CnxnConversions[Namespace,Var,Tag] {
  def cnxnLabelRuleHdStr(
    clbl : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : String = {
    val vrs = clbl.names.toSet.map(
      ( v : Either[Tag,Var] ) => {
	v match {
	  case Right( x ) => "X" + x
	  case _ => throw new Exception( "bad name" )
	}
      }
    )

    (
      cnxnNamespaceTermStr( clbl.nameSpace )
      + vrs.toString.replace(
	"Set", ""
      ).replace(
	"(", "( "
      ).replace( ")", " )" )
    )
  }

  def cnxnLabelRuleBodyStr(
    base : String,
    connective : String
  )(
    clbl : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : String = {    
    clbl.labels match {	  
      case Nil => {
	base + "." 
      }
      case juncts@_ => {
	(
	  ( juncts.take( juncts.length - 1 ) :\ "" )(
	    {
	      ( junct, acc ) => {
		(
		  cnxnCtxtLabelToTermStr( junct )
		  + connective
		  + acc
		)
	      }
	    }
	  )
	  + cnxnCtxtLabelToTermStr( juncts.last )
	  + " . "
	)
      }
    }
  }
  
  def cnxnLabelToRuleStr(
    base : String,
    connective : String
  )(
    clbl : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : String = {    
    (
      cnxnLabelRuleHdStr( clbl )
      + " :- "
      + cnxnLabelRuleBodyStr( base, connective )( clbl )
    )
  }

  def cnxnLabelToTermStr(
    clbl : CnxnCtxtConjunction[Namespace,Var,Tag]
  ) : String = {
    cnxnLabelToRuleStr( " true ", " , "  )( clbl )
  }
  def cnxnLabelToTermStr(
    clbl : CnxnCtxtDisjunction[Namespace,Var,Tag]
  ) : String = {
    cnxnLabelToRuleStr( " false ", " ; "  )( clbl )
  }
  def cnxnLabelToTermStr(
    clbl : CnxnRule[Namespace,Var,Tag]
  ) : String = {
    (
      cnxnCtxtLabelToTermStr( clbl.consequent )
      + " :- "
      + cnxnLabelRuleBodyStr( " true ", " , " )(
	  new CnxnCtxtConjunction[Namespace,Var,Tag](
	    clbl.nameSpace,
	    clbl.antecedents
	  )
	)
    )
  }
  def cnxnLabelToTermStr(
    clbl : CnxnTheory[Namespace,Var,Tag]
  ) : String = {
    ( ( "" /: clbl.facts )(
      {
	( acc, c ) => {
	  acc + "\n" + cnxnCtxtLabelToTermStr( c )
	}
      }
    ) /: clbl.rules )(
      {
	( acc, c ) => {
	  acc + "\n" + cnxnCtxtLabelToTermStr( c )
	}
      }
    )
  }
}

