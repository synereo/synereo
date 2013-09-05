// -*- mode: Scala;-*- 
// Filename:    CnxnQuery.scala 
// Authors:     lgm                                                    
// Creation:    Thu Aug 26 00:54:38 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib._

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

import org.prolog4j._

import java.net.URI

class UnificationUnsupportedException( msg : String )
extends Exception( msg )

trait CnxnConversions[Namespace,Var,Tag] {
  def cnxnNamespaceTermStr( nspace : Namespace )
  : String = {
    (nspace + "").replace( "'", "" ).replace( "$", "_" )
  }

  def cnxnTrgtTermStr( trgt : Tag )
  : String = {
    "tag( " + trgt + " )"
  }

  def cnxnTrgtTermStr( useTag : Boolean )( trgt : Either[Tag,Var] )
  : String = {
    val tagStr =
      (trgt match {
	case Left( t ) => {
	  t match {
	    case s : String => {
	      "\"" + s + "\""
	    }
	    case _ => t + ""
	  }
	}
	case Right( v ) => cnxnVarTermStr( v )
      }
     ).replace( "?", "&#161;" )
    useTag match {
      case true => {
	(
	  "tag( "
	  + tagStr
	  + " )"
	)
      }
      case _ => {
	tagStr
      }
    }    
  }

  def cnxnTrgtTermStr( trgt : Either[Tag,Var] ) : String = {
    cnxnTrgtTermStr( false )( trgt )
  }
  
  def cnxnVarTermStr( variable : Var )
  : String = {
    import java.util.UUID
    variable + "" match {
      case "_" => "X" + ( UUID.randomUUID + "" ).replace( "-", "" ) + "_"
      case _ => ("X" + variable).replace( "'", "" )
    }    
  }

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
      case _ => "\"\""
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
	      //+ "("
	      //+ " "
	      //+ ")"
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

trait PrologMgr {
  def getProver() = LocalProverFactoryPool.getProver()

  def dropProver(prover : Prover) = LocalProverFactoryPool.dropProver(prover)

  def unifyQuery(
    qStr1 : String,
    qStr2 : String
  ) : Solution[Object] = {
    val prover = getProver()
    val soln : Solution[Object] =
      prover.solve( qStr1 + " = " + qStr2 + "." )
    dropProver( prover )
    soln
  }  
}

trait PrologTermQuery[Namespace,Var,Tag] {
  self : CnxnConversions[Namespace,Var,Tag] with UUIDOps with PrologMgr =>  
  def matches(
    clabel1 : CnxnLabel[Namespace,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) :
    Option[Solution[Object]]
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
  ) : Option[Solution[Object]]

  def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[Solution[Object]]

}

trait CnxnUnificationTermQuery[Namespace,Var,Tag]
extends PrologTermQuery[Namespace,Var,Tag] 
with PrologMgr {
  self : CnxnConversions[Namespace,Var,Tag] with UUIDOps =>               

  def toNameSpace( str : String ) : Namespace = {
    // BUGBUG -- lgm : should at least report to log
    //println( "warning: coercing String, " + str + " to Namespace" )
    BasicLogService.tweet( "warning: coercing String, " + str + " to Namespace" )
    str.asInstanceOf[Namespace]
  }
  def asCnxnCtxtLabel(
    obj : Object
  ) : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    obj match {
      case cmpnd : Compound => {
	new CnxnCtxtBranch[Namespace,Var,Tag](
	  toNameSpace( cmpnd.getFunctor ),
	  cmpnd.getArgs.map( asCnxnCtxtLabel ).toList
	)
      }
      case atom => {
	new CnxnCtxtLeaf[Namespace,Var,Tag](
	  Left[Tag,Var]( atom.asInstanceOf[Tag] )
	)
      }
    }    
  }
  def matchesOne(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[Object]] = {
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
  ) : Option[Solution[Object]]
    = {      
      matchesOne( clabel1, clabel2 )      
    }
  
  def patternVars(
    pattern : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : List[Var] = {
    ( ( Nil : List[Var] ) /: pattern.atoms )(
      ( acc : List[Var], atom : Either[Tag,Var] ) => {
	atom match {
	  case Right( v ) => acc ++ List( v )
	  case _ => acc
	}
      }
    )
  }  

  def matchMap(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]] = {
    // println(
//       (
// 	"in matchMap with\n clabel1 : "
// 	+ clabel1
// 	+ "\n clabel2 : "
// 	+ clabel2
//       )
//     )
    
    // BUGBUG -- lgm : is this a reasonable optimization to do at this level?
    if ( clabel1.equals( clabel2 ) ) {
      Some( new LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]() )
    }
    else {
      val prover = getProver()
      val solution : Solution[Object] =
	prover.solve(
	  cnxnCtxtLabelToTermStr( clabel1 ) + " = " + cnxnCtxtLabelToTermStr( clabel2 ) + "."
	)
      dropProver( prover )
      
      if ( solution.isSuccess ) {
	val clbl1Vars = patternVars( clabel1 ).toSet
	val clbl2Vars = patternVars( clabel2 ).toSet
	val varSet = clbl1Vars ++ clbl2Vars
	
	val hmSoln = new LinkedHashMap[Var,CnxnCtxtLabel[Namespace,Var,Tag]]()
	
	for( v <- varSet ) {
	  //println( "mapping free var : " + v )
	  try {
	    val sV =
	      v + "" match {
		case "_" => "_"
		case _ => "X" + v
	      }
	    val soln : Solution[Object] = solution.on( sV )
	    hmSoln += ( v -> asCnxnCtxtLabel( soln.get ) )
	  }
	  catch {
	    case e : org.prolog4j.UnknownVariableException => {
//	      println( "warning: variable not bound: " + v )
	    }
	  }
	}
	
	Some( hmSoln )
      }
      else {
	None
      }
    }
  }

  override def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnCtxtLabel[Namespace,Var,Tag]
  ) : Option[Solution[Object]]
  = {
    //println( "in matches with " + clabel1 + " and " + clabel2 )
    val solution =
      unifyQuery(
	cnxnCtxtLabelToTermStr( clabel1 ),
	cnxnCtxtLabelToTermStr( clabel2 )
      )

    if ( solution.isSuccess ) {
      //println( " found a solution in matches for " + clabel1 + " and " + clabel2 )
      // BUGBUG -- fix this      
      
      Some( solution )
    }
    else {
      //println( " no solution in matches for " + clabel1 + " and " + clabel2 )
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
  ) : Option[Solution[Object]] = {
    def loopAnd(
      acc : Option[Solution[Object]],
      lbls : List[CnxnCtxtLabel[Namespace,Var,Tag]]
    ) : Option[Solution[Object]] = {
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
  ) : Option[Solution[Object]] = {
    def loopOr(
      acc : Option[Solution[Object]],
      lbls : List[CnxnCtxtLabel[Namespace,Var,Tag]]
    ) : Option[Solution[Object]] = {
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
  ) : Option[Solution[Object]]
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
    clabel1 : CnxnRule[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[Object]] = {
    // To do
    None
  }

  def doMatching(
    clabel1 : CnxnTheory[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[Object]] = {
    // To do
    None
  }

  override def matches(
    clabel1 : CnxnCtxtLabel[Namespace,Var,Tag], 
    clabel2 : CnxnLabel[Namespace,Tag]
  ) : Option[Solution[Object]]
    = {
      
      clabel1 match {
	case ccc : CnxnCtxtConjunction[Namespace,Var,Tag] => {
	  doMatching( ccc, clabel2 )	  
	}
	case ccd : CnxnCtxtDisjunction[Namespace,Var,Tag] => {
	  doMatching( ccd, clabel2 )
	}
	case ccr : CnxnRule[Namespace,Var,Tag] => {
	  doMatching( ccr, clabel2 )
	}
	case cct : CnxnTheory[Namespace,Var,Tag] => {
	  doMatching( cct, clabel2 )
	}
	case _ => {
	  matchesOne( clabel1, clabel2 )
	}
      }      
    }
}

package usage {
  import java.util.UUID
  object CnxnCtxtLabelMatcher
    extends CnxnUnificationTermQuery[String,String,String]
    with CnxnConversions[String,String,String] with UUIDOps
  object LabelStream {
    def tStream[T]( seed : T )( fresh : T => T ) : Stream[T] = {
      lazy val loopStrm : Stream[T] =
	( List( seed ) ).toStream append ( loopStrm map fresh );
      loopStrm
    }
    implicit val useUUID : Boolean = false
    def strings( 
      seed : String
    )( implicit useUUID : Boolean ) : Stream[String] = {
      def freshString( 
	s : String 
      ) : String = {
	useUUID match {
	  case true => {
	    s + UUID.randomUUID.toString.replace( "-", "" )
	  }
	  case false => {
	    val a = s.split( seed )
	    seed + ( a( 1 ).toInt + 1 )
	  }
	}
      }
      val suffix =
	useUUID match {
	  case true => {
	    UUID.randomUUID.toString.replace( "-", "" )
	  }
	  case false => {
	    1 + ""
	  }
	}
      tStream[String]( seed + suffix )( freshString )
    }
    def atoms(
      seed : String
    ) : Stream[CnxnCtxtLabel[String,String,String] with Factual] = {
      def freshTree(
	tree : CnxnCtxtLabel[String,String,String] with Factual
      ) : CnxnCtxtLabel[String,String,String] with Factual = {	
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String](
	    ( UUID.randomUUID.toString.replace( "-", "" ) + seed )
	  )
	)
      }
      tStream[CnxnCtxtLabel[String,String,String] with Factual](
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String](
	    ( UUID.randomUUID.toString.replace( "-", "" ) + seed )
	  )
	)
      )( freshTree )
    }
    def identifiers(
      seed : String
    ) : Stream[CnxnCtxtLabel[String,String,String] with Factual] = {
      def freshTree(
	tree : CnxnCtxtLabel[String,String,String] with Factual
      ) : CnxnCtxtLabel[String,String,String] with Factual = {	
	new CnxnCtxtLeaf[String,String,String](
	  Right[String,String](
	    ( "X" + UUID.randomUUID.toString.replace( "-", "" ) + seed )
	  )
	)
      }
      tStream[CnxnCtxtLabel[String,String,String] with Factual](
	new CnxnCtxtLeaf[String,String,String](
	  Right[String,String](
	    ( "X" + UUID.randomUUID.toString.replace( "-", "" ) + seed )
	  )
	)
      )( freshTree )
    }
    def facts(
      seed : String
    ) : Stream[CnxnCtxtLabel[String,String,String] with Factual] = {
      def freshTree(
	tree : CnxnCtxtLabel[String,String,String] with Factual
      ) : CnxnCtxtLabel[String,String,String] with Factual = {	
	new CnxnCtxtBranch[String,String,String](
	  ( "t" + UUID.randomUUID.toString.replace( "-", "" ) ),
	  List(
	    new CnxnCtxtBranch[String,String,String](
	      ( "tLeft" + UUID.randomUUID.toString.replace( "-", "" ) ),
	      List[CnxnCtxtLabel[String,String,String] with Factual](
		tree,
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( ( UUID.randomUUID + seed ) )
		)
	      )
	    ),
	    new CnxnCtxtBranch[String,String,String](
	      ( "tRight" + UUID.randomUUID.toString.replace( "-", "" ) ),
	      List[CnxnCtxtLabel[String,String,String] with Factual](
		new CnxnCtxtLeaf[String,String,String](
		  Left[String,String]( ( UUID.randomUUID + seed ) )
		),
		tree		
	      )
	    )
	  )
	)
      }
      tStream[CnxnCtxtLabel[String,String,String] with Factual](
	new CnxnCtxtLeaf[String,String,String](
	  Left[String,String]( ( UUID.randomUUID + seed ) )
	)
      )( freshTree )
    }

    def queries(
    ) : Stream[CnxnCtxtLabel[String,String,String] with Factual] = {
      def freshTree(
	tree : CnxnCtxtLabel[String,String,String] with Factual
      ) : CnxnCtxtLabel[String,String,String] with Factual = {	
	new CnxnCtxtBranch[String,String,String](
	  ( "t" + UUID.randomUUID.toString.replace( "-", "" ) ),
	  List(
	    new CnxnCtxtBranch[String,String,String](
	      ( "tLeft" + UUID.randomUUID.toString.replace( "-", "" ) ),
	      List[CnxnCtxtLabel[String,String,String] with Factual](
		tree,
		new CnxnCtxtLeaf[String,String,String](
		  Right[String,String]( ( "X" + UUID.randomUUID.toString.replace( "-", "" ) ) )
		)
	      )
	    ),
	    new CnxnCtxtBranch[String,String,String](
	      ( "tRight" + UUID.randomUUID.toString.replace( "-", "" ) ),
	      List[CnxnCtxtLabel[String,String,String] with Factual](
		new CnxnCtxtLeaf[String,String,String](
		  Right[String,String]( ( "X" + UUID.randomUUID.toString.replace( "-", "" ) ) )
		),
		tree		
	      )
	    )
	  )
	)
      }
      tStream[CnxnCtxtLabel[String,String,String] with Factual](
	new CnxnCtxtLeaf[String,String,String](
	  Right[String,String](
	    ( "X" + UUID.randomUUID.toString.replace( "-", "" ) )
	  )
	)
      )( freshTree )
    }
  }
}


