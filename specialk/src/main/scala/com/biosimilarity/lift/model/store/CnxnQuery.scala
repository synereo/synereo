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

class Binding[Namespace,Var,Tag](
  map : scala.collection.Map[Var,Either[Var,Either[Namespace,Tag]]]
) extends scala.collection.MapProxy[Var,Either[Var,Either[Namespace,Tag]]] {
  override def self = map
}

trait CnxnQuery[Namespace,Var,Tag] {
  def initalMatchConstraints() : Binding[Namespace,Var,Tag] = {
    new Binding[Namespace,Var,Tag](
      new scala.collection.mutable.HashMap[Var,Either[Var,Either[Namespace,Tag]]]()
    )
  }

  def matches(
    cpath1 : CnxnPath[Namespace,Tag], 
    cpath2 : CnxnPath[Namespace,Tag]
  ) : Option[(CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])]

  def matches(
    clabel1 : CnxnLabel[Namespace,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]]

  def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[String]]
  
  def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[Solution[String]]

  def namespacesUnify(
    nspace1 : Namespace,
    nspace2 : Namespace
  ) : Boolean
  def tagsUnify(
    tag1 : Tag,
    tag2 : Tag
  ) : Boolean
  def varsUnify(
    var1 : Var,
    var2 : Var,
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[Binding[Namespace,Var,Tag]]
  def varTagUnify(
    tvar : Var,
    tag : Tag,
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[Binding[Namespace,Var,Tag]]
  def varNamespaceUnify(
    tvar : Var,
    nspace : Namespace,
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[Binding[Namespace,Var,Tag]]
  
  def unifies(
    cpath1 : CnxnLiteralPath[Namespace,Tag], 
    cpath2 : CnxnLiteralPath[Namespace,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[(CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])]
  def unifies(
    cpath1 : CnxnVarPath[Namespace,Var,Tag], 
    cpath2 : CnxnLiteralPath[Namespace,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[(CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])]
  def unifies(
    cpath1 : CnxnVarPath[Namespace,Var,Tag], 
    cpath2 : CnxnVarPath[Namespace,Var,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[(CnxnVarPath[Namespace,Var,Tag],Option[Binding[Namespace,Var,Tag]])]
  def unifies(
    cpath1 : CnxnPredPath[Namespace,Var,Tag], 
    cpath2 : CnxnLiteralPath[Namespace,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[(CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])]
  def unifies(
    cpath1 : CnxnPredPath[Namespace,Var,Tag], 
    cpath2 : CnxnVarPath[Namespace,Var,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[(Option[CnxnPath[Namespace,Tag]],Option[Binding[Namespace,Var,Tag]])]
}

trait PrologMgr {
  def getProver() = { ProverFactory.getProver() }
}

trait PrologQuery[Namespace,Var,Tag] {
  self : UUIDOps with PrologMgr =>        
  def cnxnPathToTermString(
    cpath : CnxnPath[Namespace,Tag]
  ) : String = {
    cpath match {
      case litPath : CnxnLiteralPath[Namespace,Tag] => {
	cnxnPathTermStr( litPath )
      }
      case varPath : CnxnVarPath[Namespace,Var,Tag] => {
	cnxnPathTermStr( varPath )
      }
      case predPath : CnxnPredPath[Namespace,Var,Tag] => {
	cnxnPathTermStr( predPath )
      }
    }
  }

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
  def cnxnTrgtTermStr( trgt : Predicate[Namespace,Var,Tag] )
  : String = {
    (
      "tag( "
      + (trgt match {
	  case Predicate( p ) => {
	    p match {
	      case Left( nspace ) => cnxnNamespaceTermStr( nspace )
	      case Right( v ) => cnxnPredTermStr( trgt )
	    }
	  }
	}
       )
      + " )"
    )
  }

  def cnxnVarTermStr( variable : Var )
  : String = {
    "X" + variable
  }

  def cnxnPredTermStr( pred : Predicate[Namespace,Var,Tag] )
  : String = {
    "Fn_" + pred
  }

  def cnxnPathElementTermStr( elem : Namespace ) = {
    cnxnNamespaceTermStr( elem )
  }
  def cnxnPathElementTermStr( elem : Either[Namespace,Var] ) = {
    elem match {
      case Left( nspace ) => cnxnNamespaceTermStr( nspace )
      case Right( v ) => cnxnVarTermStr( v ) 
    }
  }
  def cnxnPathElementTermStr( elem : Predicate[Namespace,Var,Tag] ) = {    
    elem match {
      case Predicate( p ) => {
	p match {
	  case Left( nspace ) => cnxnNamespaceTermStr( nspace )
	  case Right( v ) => cnxnPredTermStr( elem ) 
	}
      }
    }
  }
    
  def cnxnPathTermStr( cpath : CnxnLiteralPath[Namespace,Tag] ) : String
  def cnxnPathTermStr( cpath : CnxnVarPath[Namespace,Var,Tag] ) : String
  def cnxnPathTermStr( cpath : CnxnPredPath[Namespace,Var,Tag] ) : String

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

}

trait FlatPrologQuery[Namespace,Var,Tag] 
extends PrologQuery[Namespace,Var,Tag] {
  self : UUIDOps with PrologMgr =>    
  
  def pathPrefixStr [Elem] ( elems : Seq[Elem] ) = {
    ( "path( " /: elems )( 
	{ 
	  ( acc, e ) => {
	    (
	      acc
	      + (e match {
		case v : Either[Namespace,Var] => {
		  println( "found a namespace/var" )
		  cnxnPathElementTermStr( v )
		}
		case p : Predicate[Namespace,Var,Tag] => {
		  println( "found a predicate" )
		  cnxnPathElementTermStr( p )
		}
		case nspace : Namespace => {
		  println( "found a namespace" )
		  cnxnPathElementTermStr( nspace )
		}		
		case _ => throw new Exception( "unexpected path element type " )
	      })
	      + " , "
	    )
	  }
	}
    )
  }
  def trgtSuffixStr [Trgt] ( trgt : Trgt ) : String = {
    trgt match {
      case v : Either[Tag,Var] =>
	cnxnTrgtTermStr( v ) + " )"
      case p : Predicate[Namespace,Var,Tag] =>
	cnxnTrgtTermStr( p ) + " )"
      case tag : Tag =>
	cnxnTrgtTermStr( tag ) + " )"      
    }
  }

  def cnxnPathTermStr( cpath : CnxnLiteralPath[Namespace,Tag] )
  : String = {
    pathPrefixStr( cpath.path ) + trgtSuffixStr( cpath.trgt )
  }

  def cnxnPathTermStr( cpath : CnxnVarPath[Namespace,Var,Tag] )
  : String = {
    pathPrefixStr( cpath.path ) + trgtSuffixStr( cpath.trgt )
  }
  def cnxnPathTermStr( cpath : CnxnPredPath[Namespace,Var,Tag] )
  : String = {
    pathPrefixStr( cpath.path ) + trgtSuffixStr( cpath.trgt )
  }
}

trait StructuredPrologQuery[Namespace,Var,Tag] 
extends PrologQuery[Namespace,Var,Tag] {
  self : UUIDOps with PrologMgr =>        
  
  def cnxnPathTermStr( cpath : CnxnLiteralPath[Namespace,Tag] )
  : String = {
    (  cpath.path :\ cnxnTrgtTermStr( cpath.trgt ) )( 
      { 
	( e, acc ) => {
	  cnxnPathElementTermStr( e ) + "( " + acc + " )"
	}
      }
    )
  }

  def cnxnPathTermStr( cpath : CnxnVarPath[Namespace,Var,Tag] )
  : String = {
    (  cpath.path :\ cnxnTrgtTermStr( cpath.trgt ) )( 
      { 
	( e, acc ) => {
	  cnxnPathElementTermStr( e ) + "( " + acc + " )"
	}
      }
    )
  }

  def cnxnPathTermStr( cpath : CnxnPredPath[Namespace,Var,Tag] )
  : String = {
    (  cpath.path :\ cnxnTrgtTermStr( cpath.trgt ) )( 
      { 
	( e, acc ) => {
	  cnxnPathElementTermStr( e ) + "( " + acc + " )"
	}
      }
    )
  }
}

trait CnxnUnificationQuery[Namespace,Var,Tag]
extends PrologQuery[Namespace,Var,Tag] 
with PrologMgr {
  self : CnxnQuery[Namespace,Var,Tag] with UUIDOps => 
  def unifyQuery(
    qStr1 : String,
    qStr2 : String
  ) : Solution[String] = {
    val prover = getProver()
    val queryStr = qStr1 + " = " + qStr2 + "."
    prover.solve( queryStr )
  }
  override def matches(
    cpath1 : CnxnPath[Namespace,Tag], 
    cpath2 : CnxnPath[Namespace,Tag]
  ) = {
    val solution =
      unifyQuery(
	cnxnPathToTermString( cpath1 ),
	cnxnPathToTermString( cpath2 )
      )
    if ( solution.isSuccess ) {
      // BUGBUG -- fix this
      Some( cpath1, None )
    }
    else {
      None
    }
  }

  override def matches(
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
  ) :
    Option[Solution[String]]
    = {
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
  
  def matches(
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
  
  def matches(
    cpath1 : CnxnVarPath[Namespace,Var,Tag], 
    cpath2 : CnxnPath[Namespace,Tag]
  ) = {
    val solution =
      unifyQuery(
	cnxnPathTermStr( cpath1 ),
	cnxnPathToTermString( cpath2 )
      )
    if ( solution.isSuccess ) {
      // BUGBUG -- fix this
      Some( cpath1, None )
    }
    else {
      None
    }
  }

  def matches(
    cpath1 : CnxnVarPath[Namespace,Var,Tag], 
    cpath2 : CnxnVarPath[Namespace,Var,Tag]
  ) = {
    unifies( cpath1, cpath2, None )
  }

  def matches(
    cpath1 : CnxnPredPath[Namespace,Var,Tag], 
    cpath2 : CnxnPath[Namespace,Tag]
  ) = {
    val solution =
      unifyQuery(
	cnxnPathTermStr( cpath1 ),
	cnxnPathToTermString( cpath2 )
      )
    if ( solution.isSuccess ) {
      // BUGBUG -- fix this
      Some( cpath1, None )
    }
    else {
      None
    }
  }
  
  def matches(
    cpath1 : CnxnPredPath[Namespace,Var,Tag],
    cpath2 : CnxnVarPath[Namespace,Var,Tag]    
  ) = {
    unifies( cpath1, cpath2, None )
  }
  
  override def namespacesUnify(
    nspace1 : Namespace,
    nspace2 : Namespace
  ) : Boolean = {
    unifyQuery(
      cnxnNamespaceTermStr( nspace1 ),
      cnxnNamespaceTermStr( nspace2 )
    ).isSuccess
  }
  def tagsUnify(
    tag1 : Tag,
    tag2 : Tag
  ) : Boolean = {
    unifyQuery(
      cnxnTrgtTermStr( tag1 ),
      cnxnTrgtTermStr( tag2 )
    ).isSuccess
  }
  def varsUnify(
    var1 : Var,
    var2 : Var,
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[Binding[Namespace,Var,Tag]] = {
    unifyQuery(
      cnxnVarTermStr( var1 ),
      cnxnVarTermStr( var2 )
    ).isSuccess match {
      case true => {
	Some(
	  new Binding(
	    new scala.collection.mutable.HashMap[
	      Var,
	      Either[Var,Either[Namespace,Tag]]
	    ]()
	  )
	)
      }
      case _ => None
    }
  }
  def varTagUnify(
    tvar : Var,
    tag : Tag,
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[Binding[Namespace,Var,Tag]] = {
    unifyQuery(
      cnxnVarTermStr( tvar ),
      cnxnTrgtTermStr( tag )
    ).isSuccess match {
      case true => {
	Some(
	  new Binding(
	    new scala.collection.mutable.HashMap[
	      Var,
	      Either[Var,Either[Namespace,Tag]]
	    ]()
	  )
	)
      }
      case _ => None
    }
  }
  def varNamespaceUnify(
    tvar : Var,
    nspace : Namespace,
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[Binding[Namespace,Var,Tag]] = {
    unifyQuery(
      cnxnVarTermStr( tvar ),
      cnxnNamespaceTermStr( nspace )
    ).isSuccess match {
      case true => {
	Some(
	  new Binding(
	    new scala.collection.mutable.HashMap[
	      Var,
	      Either[Var,Either[Namespace,Tag]]
	    ]()
	  )
	)
      }
      case _ => None
    }
  }
  
  def unifies(
    cpath1 : CnxnLiteralPath[Namespace,Tag], 
    cpath2 : CnxnLiteralPath[Namespace,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) : Option[
    (CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])
  ] = {
    unifyQuery(
      cnxnPathTermStr( cpath1 ),
      cnxnPathTermStr( cpath2 )
    ).isSuccess match {
      case true => {
	Some(
	  cpath1,
	  Some(
	    new Binding(
	      new scala.collection.mutable.HashMap[
		Var,
		Either[Var,Either[Namespace,Tag]]
	      ]()
	    )
	  )
	)
      }
      case _ => None
    }
  }
  def unifies(
    cpath1 : CnxnVarPath[Namespace,Var,Tag], 
    cpath2 : CnxnLiteralPath[Namespace,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) :
    Option[(CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])]
    = {
    unifyQuery(
      cnxnPathTermStr( cpath1 ),
      cnxnPathTermStr( cpath2 )
    ).isSuccess match {
      case true => {
	Some(
	  (
	    cpath2,
	    Some(
	      new Binding(
		new scala.collection.mutable.HashMap[
		  Var,
		  Either[Var,Either[Namespace,Tag]]
		]()
	      )
	    )
	  )
	)
      }
      case _ => None
    }
  }
  def unifies(
    cpath1 : CnxnVarPath[Namespace,Var,Tag], 
    cpath2 : CnxnVarPath[Namespace,Var,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) :
    Option[(CnxnVarPath[Namespace,Var,Tag],Option[Binding[Namespace,Var,Tag]])]
    = {
    unifyQuery(
      cnxnPathTermStr( cpath1 ),
      cnxnPathTermStr( cpath2 )
    ).isSuccess match {
      case true => {
	Some(
	  (
	    cpath1,
	    Some(
	      new Binding(
		new scala.collection.mutable.HashMap[
		  Var,
		  Either[Var,Either[Namespace,Tag]]
		]()	      
	      )
	    )
	  )
	)
      }
      case _ => None
    }
  }
  def unifies(
    cpath1 : CnxnPredPath[Namespace,Var,Tag], 
    cpath2 : CnxnLiteralPath[Namespace,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) :
    Option[(CnxnPath[Namespace,Tag],Option[Binding[Namespace,Var,Tag]])]
    = {
    unifyQuery(
      cnxnPathTermStr( cpath1 ),
      cnxnPathTermStr( cpath2 )
    ).isSuccess match {
      case true => {
	Some(
	  (
	    cpath2,
	    Some(
	      new Binding(
		new scala.collection.mutable.HashMap[
		  Var,
		  Either[Var,Either[Namespace,Tag]]
		]()
	      )
	    )	  
	  )
	)
      }
      case _ => None
    }
  }

  def unifies(
    cpath1 : CnxnPredPath[Namespace,Var,Tag], 
    cpath2 : CnxnVarPath[Namespace,Var,Tag],
    constraints : Option[Binding[Namespace,Var,Tag]]
  ) :
    Option[(Option[CnxnPath[Namespace,Tag]],Option[Binding[Namespace,Var,Tag]])]
    = {
    unifyQuery(
      cnxnPathTermStr( cpath1 ),
      cnxnPathTermStr( cpath2 )
    ).isSuccess match {
      case true => {
	Some(
	  (
	    None,
	    Some(
	      new Binding(
		new scala.collection.mutable.HashMap[
		  Var,
		  Either[Var,Either[Namespace,Tag]]
		]()
	      )
	    )
	  )
	)
      }
      case _ => None
    }
  }
}

class CnxnFlatUnificationQuery[Namespace,Var,Tag]()
extends CnxnUnificationQuery[Namespace,Var,Tag]
with CnxnQuery[Namespace,Var,Tag]
with FlatPrologQuery[Namespace,Var,Tag]
with UUIDOps {      
}

class CnxnStructuredUnificationQuery[Namespace,Var,Tag]()
extends CnxnUnificationQuery[Namespace,Var,Tag]
with CnxnQuery[Namespace,Var,Tag]
with StructuredPrologQuery[Namespace,Var,Tag]
with UUIDOps {      
}
