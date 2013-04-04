package com.biosimilarity.lift.lib.term.Prolog;

import com.biosimilarity.lift.lib.term.Prolog.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate.Visitor<R,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Term.Visitor<R,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom.Visitor<R,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor.Visitor<R,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole.Visitor<R,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Var.Visitor<R,A>,
  com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst.Visitor<R,A>
{}
