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

object CnxnCtxtLabelConversionScope {
  import scala.collection.JavaConversions._
  import scala.collection.JavaConverters._
  type TermToLabelMap[N,X,T] =
    HashMap[Either[Symbol,PrologPredicate],Option[Location[Either[T,X]]]]

  

  abstract class TermToCnxnCtxtLabel[N,X,T](
    val text2ns : String => N, val text2v : String => X, val text2t : String => T,
    val ns2str : N => String, val v2str : X => String, val t2str : T => String,
    val zipper : ZipperNavigation[Either[T,X]] with ZipperMutation[Either[T,X]]
  ) extends FoldVisitor[Option[Location[Either[T,X]]], TermToLabelMap[N,X,T]] {
    def wrap(
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      None
    }

    override def combine(
      x : Option[Location[Either[T,X]]], 
      y : Option[Location[Either[T,X]]], 
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      None
    }

    /* Predicate */
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.APred,
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      combine(
	p.atom_.accept( this, context ),
	wrap( context ),
	context
      )
    }
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred,
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      combine( p.atom_.accept( this, context ), wrap( context ), context)
    }
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT,
      context : TermToLabelMap[N,X,T] 
    ) : Option[Location[Either[T,X]]] = {
      combine( p.var_.accept( this, context ), wrap( context ), context)
    }
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex,
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      combine( p.lyst_.accept( this, context ), wrap( context ), context )
    }

    /* Atom */
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm,
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      combine( p.boole_.accept( this, context ), wrap( context ), context)
    }
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm,
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      /* TBD */
      None
    }

    /* Boole */
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity,
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      wrap( context )      
    }
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.A,
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      wrap( context )
    }

    /* Lyst */
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty,
      context : TermToLabelMap[N,X,T]
    ) : Option[Location[Either[T,X]]] = {
      wrap( context )
    }
    override def visit(
      p : com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum,
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
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
      context : TermToLabelMap[N,X,T]
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
}
