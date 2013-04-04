// -*- mode: Scala;-*- 
// Filename:    Cnxn.scala
// Authors:     lgm                                                    
// Creation:    Wed Apr  3 09:44:55 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.term.conversion

import com.biosimilarity.lift.lib.term.Prolog._
import com.biosimilarity.lift.lib.term.Prolog.Absyn.{Predicate => PrologPredicate,_}
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib.zipper._
import com.biosimilarity.lift.lib.navigation.{ Right => NRight, Left => NLeft,_ }

import scala.collection.mutable.HashMap

trait CnxnNavigation[L,V,T] extends ZipperNavigation[Either[T,V]] {
  override def left [A1 >: Either[T,V]] ( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "left of top" )
      }
      case Location( t, LabeledTreeContext( lbl, l :: left, up, right ) ) => {
        Location( l, LabeledTreeContext( lbl, left, up, t :: right ) )
      }
      case Location( t, LabeledTreeContext( lbl, Nil, up, right ) ) => {
        throw new Exception( "left of first" )
      }
    }
  }
  override def right [A1 >: Either[T,V]] ( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "right of top" )
      }
      case Location( t, LabeledTreeContext( lbl, left, up, r :: right ) ) => {
        Location( r, LabeledTreeContext( lbl, t :: left, up, right ) )
      }
      case Location( t, _ ) => {
        throw new Exception( "right of last" )
      }
    }
  }
  override def up [A1 >: Either[T,V]]( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( _, Top( ) ) => {
        throw new Exception( "up of top" )
      }   
      case Location( t : CnxnCtxtLabel[L,V,T] with Factual, LabeledTreeContext( lbl : L, left, up, right ) ) => {
	( left, right ) match {
	  case ( lTerms : List[CnxnCtxtLabel[L,V,T] with Factual], rTerms : List[CnxnCtxtLabel[L,V,T] with Factual] ) => {
	    val rvrsLTerms : List[CnxnCtxtLabel[L,V,T] with Factual] = lTerms.reverse
            Location[A1]( new CnxnCtxtBranch[L,V,T]( lbl, rvrsLTerms ::: ( t :: rTerms ) ), up )
	  }
	  case _ => throw new Exception( "unexpected location shape: " + location )
	}
      }
    }
  }
  override def down [A1 >: Either[T,V]]( location : Location[A1] ) : Location[A1] = {
    location match {
      case Location( TreeItem( _ ), _ ) => {
        throw new Exception( "down of item" )
      }
      case Location( TreeSection( Nil ), ctxt ) => {
        throw new Exception( "down of empty" )
      }
      case Location( CnxnCtxtBranch( lbl, u :: trees ), ctxt ) => {
        Location( u, LabeledTreeContext( lbl, Nil, ctxt, trees ) )
      }
    }
  }
}

trait CnxnMutation[L,V,T] extends ZipperMutation[Either[T,V]] {
  def update(
    location : Location[Either[T,V]],
    tree : CnxnCtxtLabel[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, ctxt ) =>
	Location( tree, ctxt )
    }
  }
  def insertRight(
    location : Location[Either[T,V]],
    tree : CnxnCtxtLabel[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "insert of top" )
      }
      case Location(
	curr,
	LabeledTreeContext( lbl, left, up, right )
      ) => {
	Location(
	  curr,
	  LabeledTreeContext( lbl, left, up, tree :: right )
	)	
      }
    }    
  }
  def insertLeft(
    location : Location[Either[T,V]], tree : CnxnCtxtLabel[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "insert of top" )
      }
      case Location(
	curr,
	LabeledTreeContext( lbl, left, up, right )
      ) => {
	Location(
	  curr,
	  LabeledTreeContext( lbl, tree :: left, up, right )
	)	
      }
    }    
  }
  def insertDown(
    location : Location[Either[T,V]], tree : CnxnCtxtLabel[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( TreeItem( _ ), _ ) => {
	throw new Exception( "down of item" )
      }
      case Location(
	CnxnCtxtBranch( lbl, progeny ),
	ctxt
      ) => {
	Location(
	  tree,
	  LabeledTreeContext( lbl, Nil, ctxt, progeny )
	)
      }
    }
  }
  def delete(
    location : Location[Either[T,V]], tree : CnxnCtxtLabel[L,V,T]
  ) : Location[Either[T,V]] = {
    location match {
      case Location( _, Top( ) ) => {
	throw new Exception( "delete of top" )
      }
      case Location(
	_,
	LabeledTreeContext( lbl, left, up, r :: right )
      ) => {
	Location(
	  r,
	  LabeledTreeContext( lbl, left, up, right )
	)
      }
      case Location(
	_,
	LabeledTreeContext( lbl, l :: left, up, Nil )
      ) => {
	Location(
	  l,
	  LabeledTreeContext( lbl, left, up, Nil )
	)
      }
      case Location(
	_,
	LabeledTreeContext( lbl : L, Nil, up, Nil )
      ) => {
	Location( new CnxnCtxtBranch[L,V,T]( lbl, Nil ), up )
      }
    }
  }
}

trait CnxnZipperComposition[L,V,T] {
  def compose(
    ctxt : Context[Either[T,V]],
    tree : CnxnCtxtLabel[L,V,T] with Factual
  ) : CnxnCtxtLabel[L,V,T] with Factual = {
    ctxt match {
      case Top() => tree
      case LabeledTreeContext( lbl : L, left : List[CnxnCtxtLabel[L,V,T] with Factual], ctxt : LabeledTreeContext[L, Either[T,V]], right : List[CnxnCtxtLabel[L,V,T] with Factual] ) => {
	new CnxnCtxtBranch[L,V,T](
	  lbl, 
	  left ++ ( compose( ctxt, tree ) :: right )
	)
      }
    }
  }
  def decontextualize( location : Location[Either[T,V]] ) : CnxnCtxtLabel[L,V,T] with Factual = {
    location match {
      case Location( tree : CnxnCtxtLabel[L,V,T] with Factual, ctxt ) => {
	compose( ctxt, tree )
      }
      case _ => throw new Exception( "unexpected location shape: " + location )
    }
    
  }
}

class TermToCnxnCtxtLabel[N,X,T](
  val text2ns : String => N, val text2v : String => X, val text2t : String => T,
  val ns2str : N => String, val v2str : X => String, val t2str : T => String,
  val zipr : CnxnNavigation[N,X,T] with CnxnMutation[N,X,T] with CnxnZipperComposition[N,X,T]
) extends FoldVisitor[Option[Location[Either[T,X]]], Option[Location[Either[T,X]]]] {
  import scala.collection.JavaConversions._
  import scala.collection.JavaConverters._
  
  def wrap(
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    context
  }
  def leaf(
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    context
  }
  
  override def combine(
    x : Option[Location[Either[T,X]]], 
    y : Option[Location[Either[T,X]]], 
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    for( 
      Location( xTerm, Top() ) <- x;
      yCCL <- y
    ) yield {
      zipr.insertDown( yCCL, xTerm )
    }
  }
  
  /* Predicate */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.APred,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    combine(
      p.atom_.accept( this, context ),
      wrap( context ),
      context
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {      
    val loc =
      combine( p.functor_.accept( this, context ), wrap( context ), context )
    val termListTrampoline1 : java.util.List[Term] = p.listterm_
    val termListTrampoline2 : scala.collection.mutable.Buffer[Term] =
      termListTrampoline1
    val terms : List[Term] = termListTrampoline2.toList
    
    ( loc /: terms )(
      { 
	( acc, e ) => {
	  combine( e.accept( this, context ), acc, context )
	}
      }
    )
  }
  
  /* Term */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    combine( p.atom_.accept( this, context ), wrap( context ), context)
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT,
    context : Option[Location[Either[T,X]]] 
  ) : Option[Location[Either[T,X]]] = {
    combine( p.var_.accept( this, context ), wrap( context ), context)
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    val loc =
      combine( p.atom_.accept( this, context ), wrap( context ), context )
    val termListTrampoline1 : java.util.List[Term] = p.listterm_
    val termListTrampoline2 : scala.collection.mutable.Buffer[Term] =
      termListTrampoline1
    val terms : List[Term] = termListTrampoline2.toList
    
    ( loc /: terms )(
      { 
	( acc, e ) => {
	  combine( e.accept( this, context ), acc, context )
	}
      }
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.TList,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    combine( p.lyst_.accept( this, context ), wrap( context ), context )
  }
  
  /* Atom */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( p.lident_ ) )
	),
	Top()
      )
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( p.ident_ ) )
	),
	Top()
      )
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    combine( p.boole_.accept( this, context ), wrap( context ), context)
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( p.string_ ) )
	),
	Top()
      )
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( p.integer_.toString ) )
	),
	Top()
      )
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( p.double_.toString ) )
	),
	Top()
      )
    )
  }
  
  /* Functor */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtBranch[N,X,T](
	  text2ns( p.lident_ ),
	  List[CnxnCtxtLabel[N,X,T] with Factual]()
	),
	Top()
      )
    )
  }
  
  /* Boole */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( "true" ) )
	),
	Top()
      )
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    /* TBD */
    Some(
      Location[Either[T,X]](
	new CnxnCtxtLeaf[N,X,T](
	  Left[T,X]( text2t( "false" ) )
	),
	Top()
      )
    )
  }
  
  /* Var */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.V,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    wrap( context )      
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.A,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    wrap( context )
  }
  
  /* Lyst */
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    wrap( context )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    val termListTrampoline1 : java.util.List[Term] = p.listterm_
    val termListTrampoline2 : scala.collection.mutable.Buffer[Term] =
      termListTrampoline1
    val terms : List[Term] = termListTrampoline2.toList
    
    ( wrap( context ) /: terms )(
      { 
	( acc, e ) => {
	  combine( e.accept( this, context ), acc, context )
	}
      }
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    val termListTrampoline1 : java.util.List[Term] = p.listterm_
    val termListTrampoline2 : scala.collection.mutable.Buffer[Term] =
      termListTrampoline1
    val terms : List[Term] = termListTrampoline2.toList
    
    combine(
      p.lyst_.accept( this, context ),
      ( wrap( context ) /: terms )(
	{ 
	  ( acc, e ) => {
	    combine( e.accept( this, context ), acc, context )
	  }
	}
      ),
      context 
    )
  }
  override def visit(
    p : com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV,
    context : Option[Location[Either[T,X]]]
  ) : Option[Location[Either[T,X]]] = {
    val termListTrampoline1 : java.util.List[Term] = p.listterm_
    val termListTrampoline2 : scala.collection.mutable.Buffer[Term] =
      termListTrampoline1
    val terms : List[Term] = termListTrampoline2.toList
    
    combine(
      p.var_.accept( this, context ),
      ( wrap( context ) /: terms )(
	{ 
	  ( acc, e ) => {
	    combine( e.accept( this, context ), acc, context )
	  }
	}
      ),
      context
    )
  }
}

object TermToCnxnCtxtLabel {
  def apply [N,X,T] (
    text2ns : String => N, text2v : String => X, text2t : String => T,
    ns2str : N => String, v2str : X => String, t2str : T => String,
    zipr : CnxnNavigation[N,X,T] with CnxnMutation[N,X,T] with CnxnZipperComposition[N,X,T]
  ) : TermToCnxnCtxtLabel[N,X,T] = {
    new TermToCnxnCtxtLabel[N,X,T](
      text2ns, text2v, text2t,
      ns2str, v2str, t2str,
      zipr
    )
  }
  
  def unapply [N,X,T] (
    xform : TermToCnxnCtxtLabel[N,X,T]
  ) : Option[(
    String => N, String => X, String => T, N => String,
    X => String, T => String,
    CnxnNavigation[N,X,T] with CnxnMutation[N,X,T] with CnxnZipperComposition[N,X,T]
  )] = {
    Some(
      (
	xform.text2ns, xform.text2v, xform.text2t,
	xform.ns2str, xform.v2str, xform.t2str,
	xform.zipr
      )
    )
  }
}

package usage {
  case class Identity[T]( ) extends Function1[T,T] {
    override def apply( t : T ) = t
  }
  object idS extends Identity[String]
  object CnxnStrZipr
  extends CnxnNavigation[String,String,String]
  with CnxnMutation[String,String,String]
  with CnxnZipperComposition[String,String,String]

  case class TermToCCLStr( ) extends TermToCnxnCtxtLabel(
    idS, idS, idS, idS, idS, idS, CnxnStrZipr    
  ) {
    def strToTerm( s : String ) : CnxnCtxtLabel[String,String,String] = {
      val ast = 
	new parser( new Yylex( new java.io.StringReader( s ) ) ).pPredicate()
      val loc : Location[Either[String,String]] =
	Location[Either[String,String]]( 
	  new CnxnCtxtBranch[String,String,String]( "root", Nil ),
	  Top()
	)
      val ctxt : Option[Location[Either[String,String]]] = Some( loc )

      val xformedTerm =
	ast match {
	  case apred : APred => visit( apred, ctxt )
	  case cpred : CPred => visit( cpred, ctxt )
	}

      xformedTerm match {
	case Some( loc ) => zipr.decontextualize( loc )
	case None => throw new Exception( "xform failed: " + ast )
      }
    }
  }
}
