// -*- mode: Scala;-*- 
// Filename:    CnxnQuery.scala 
// Authors:     lgm                                                    
// Creation:    Thu Aug 26 00:54:38 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.lib._

import org.prolog4j._

import java.net.URI

class UnificationUnsupportedException( msg : String )
extends Exception( msg )

trait CnxnConversions[Namespace,Var,Tag] {
  def cnxnNamespaceTermStr( nspace : Namespace )
  : String = {
    nspace + ""
  }

  def cnxnTrgtTermStr( trgt : Tag )
  : String = {
    "tag( " + trgt + " )"
  }

  def cnxnTrgtTermStr( trgt : Either[Tag,Var] )
  : String = {
    (
      "tag( "
      + (trgt match {
	  case Left( t ) => t
	  case Right( v ) => cnxnVarTermStr( v )
	}
      )
      + " )"
    )
  }  
  
  def cnxnVarTermStr( variable : Var )
  : String = {
    "X" + variable
  }
}

trait PrologTermQuery[Namespace,Var,Tag] {
  self : CnxnConversions[Namespace,Var,Tag] with UUIDOps with PrologMgr =>            

  def cnxnLabelToTermStr(
    clbl : CnxnLabel[Namespace,Tag]
  ) : String = {
    clbl match {
      case cLeaf : CnxnLeaf[Namespace,Tag] =>
	cnxnLabelTermStr( cLeaf )
      case cBranch : CnxnBranch[Namespace,Tag] =>
	cnxnLabelTermStr( cBranch )
    }
  }
  def cnxnCtxtLabelToTermStr(
    cclbl : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : String = {
    cclbl match {
      case cLeaf : CnxnCtxtLeaf[Namespace,Var,Tag] =>
	cnxnLabelTermStr( cLeaf )
      case cBranch : CnxnCtxtBranch[Namespace,Var,Tag] =>
	cnxnLabelTermStr( cBranch )
    }
  }

  def cnxnLabelTermStr( clbl : CnxnLeaf[Namespace,Tag] ) : String = {
    cnxnTrgtTermStr( clbl.tag )
  }
  def cnxnLabelTermStr(
    clbl : CnxnBranch[Namespace,Tag]
  ) : String = {
    clbl match {
      case CnxnBranch( nspace, labels ) => {
	labels match {
	  case Nil => {
	    (
	      cnxnNamespaceTermStr( nspace ) 
	      + "("
	      + " "
	      + ")"
	    )
	  }
	  case lbl :: lbls => {
	    (
	      cnxnNamespaceTermStr( nspace ) 
	      + "( "
	      + ( cnxnLabelToTermStr( lbl ) /: lbls )(
		{
		  ( acc, e ) => {
		    acc + " , " + cnxnLabelToTermStr( e ) 
		  }
		}
	      )
	      + " )"
	    )
	  }
	}
      }
    }
  }

  def cnxnLabelTermStr(
    clbl : CnxnCtxtLeaf[Namespace,Var,Tag]
  ) : String = {
    cnxnTrgtTermStr( clbl.tag )
  }
  def cnxnLabelTermStr(
    clbl : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : String = {
    clbl match {
      case CnxnCtxtBranch( nspace, labels ) => {
	labels match {
	  case Nil => {
	    (
	      cnxnNamespaceTermStr( nspace ) 
	      + "("
	      + " "
	      + ")"
	    )
	  }
	  case lbl :: lbls => {
	    (
	      cnxnNamespaceTermStr( nspace ) 
	      + "( "
	      + ( cnxnCtxtLabelToTermStr( lbl ) /: lbls )(
		{
		  ( acc, e ) => {
		    acc + " , " + cnxnCtxtLabelToTermStr( e ) 
		  }
		}
	      )
	      + " )"
	    )
	  }
	}
      }
    }
  }  

  def unifyQuery(
    qStr1 : String,
    qStr2 : String
  ) : Solution[String] = {
    val prover = getProver()
    val queryStr = qStr1 + " = " + qStr2 + "."
    prover.solve( queryStr )
  }

  def matches(
    clabel1 : CnxnLabel[Namespace,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) :
    Option[Solution[String]]
    = {
    val solution =
      unifyQuery(
	cnxnLabelToTermStr( clabel1 ),
	cnxnLabelToTermStr( clabel2 )
      )
    if ( solution.isSuccess ) {
      // BUGBUG -- fix this
      Some( solution )
    }
    else {
      None
    }
  }

  def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]]

  def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[Solution[String]]

}

trait CnxnUnificationTermQuery[Namespace,Var,Tag]
extends PrologTermQuery[Namespace,Var,Tag] 
with PrologMgr {
  self : CnxnConversions[Namespace,Var,Tag] with UUIDOps =>               
  def matchesOne(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]] = {
    val solution =
      unifyQuery(
	cnxnCtxtLabelToTermStr( clabel1 ),
	cnxnLabelToTermStr( clabel2 )
      )
    if ( solution.isSuccess ) {
      // BUGBUG -- fix this
      Some( solution )
    }
    else {
      None
    }
  }

  override def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]]
    = {      
      matchesOne( clabel1, clabel2 )      
    }
  
  override def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnCtxtLabel[Namespace,Var,Tag]
  ) :
    Option[Solution[String]]
    = {
    val solution =
      unifyQuery(
	cnxnCtxtLabelToTermStr( clabel1 ),
	cnxnCtxtLabelToTermStr( clabel2 )
      )
    if ( solution.isSuccess ) {
      // BUGBUG -- fix this
      Some( solution )
    }
    else {
      None
    }
  }
}

trait CnxnUnificationCompositeTermQuery[Namespace,Var,Tag]
extends CnxnUnificationTermQuery[Namespace,Var,Tag] 
with PrologMgr {
  self : CnxnConversions[Namespace,Var,Tag] with UUIDOps =>         
  def doMatching(
    clabel1 : CnxnCtxtConjunction[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]] = {
    def loopAnd(
      acc : Option[Solution[String]],
      lbls : List[CnxnCtxtLabel[Namespace,Var,Tag]]
    ) : Option[Solution[String]] = {
      lbls match {
	case lbl :: rlbls => {
	  // BUGBUG -- lgm -- must compose solutions...
	  matchesOne( lbl, clabel2 ) match {
	    case optSol@Some( s ) => loopAnd( optSol, rlbls )
	    case None => None
	  }
	}
	case Nil => acc
      }
    }    
    loopAnd( None, clabel1.labels )
  }

  def doMatching(
    clabel1 : CnxnCtxtDisjunction[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]] = {
    def loopOr(
      acc : Option[Solution[String]],
      lbls : List[CnxnCtxtLabel[Namespace,Var,Tag]]
    ) : Option[Solution[String]] = {
      lbls match {
	case lbl :: rlbls => {
	  // BUGBUG -- lgm -- must compose solutions...
	  matchesOne( lbl, clabel2 ) match {
	    case optSol@Some( s ) => optSol
	    case None => loopOr( None, rlbls )
	  }
	}
	case Nil => acc
      }
    }
    loopOr( None, clabel1.labels )
  }  

  override def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]]
    = {      
      clabel1 match {
	case ccc : CnxnCtxtConjunction[Namespace,Var,Tag] => {
	  doMatching( ccc, clabel2 )	  
	}
	case ccd : CnxnCtxtDisjunction[Namespace,Var,Tag] => {
	  doMatching( ccd, clabel2 )
	}
	case _ => {
	  matchesOne( clabel1, clabel2 )
	}
      }      
    }
}

trait CnxnTheoryQuery[Namespace,Var,Tag]
extends CnxnUnificationCompositeTermQuery[Namespace,Var,Tag] 
with PrologMgr {
  self : CnxnConversions[Namespace,Var,Tag] with UUIDOps =>         
  def doMatching(
    clabel1 : CnxnCtxtRule[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]] = {
    // To do
    None
  }

  def doMatching(
    clabel1 : CnxnCtxtTheory[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]] = {
    // To do
    None
  }

  override def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]]
    = {
      
      clabel1 match {
	case ccc : CnxnCtxtConjunction[Namespace,Var,Tag] => {
	  doMatching( ccc, clabel2 )	  
	}
	case ccd : CnxnCtxtDisjunction[Namespace,Var,Tag] => {
	  doMatching( ccd, clabel2 )
	}
	case ccr : CnxnCtxtRule[Namespace,Var,Tag] => {
	  doMatching( ccr, clabel2 )
	}
	case cct : CnxnCtxtTheory[Namespace,Var,Tag] => {
	  doMatching( cct, clabel2 )
	}
	case _ => {
	  matchesOne( clabel1, clabel2 )
	}
      }      
    }
}


