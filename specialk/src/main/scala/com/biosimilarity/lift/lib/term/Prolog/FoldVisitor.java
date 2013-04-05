package com.biosimilarity.lift.lib.term.Prolog;

import com.biosimilarity.lift.lib.term.Prolog.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Predicate */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.APred p, A arg) {
      R r = leaf(arg);
      r = combine(p.atom_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred p, A arg) {
      R r = leaf(arg);
      r = combine(p.functor_.accept(this, arg), r, arg);
      for (Term x : p.listterm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* Term */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom p, A arg) {
      R r = leaf(arg);
      r = combine(p.atom_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT p, A arg) {
      R r = leaf(arg);
      r = combine(p.var_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex p, A arg) {
      R r = leaf(arg);
      r = combine(p.functor_.accept(this, arg), r, arg);
      for (Term x : p.listterm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TList p, A arg) {
      R r = leaf(arg);
      r = combine(p.lyst_.accept(this, arg), r, arg);
      return r;
    }

/* Atom */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm p, A arg) {
      R r = leaf(arg);
      r = combine(p.boole_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Functor */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Boole */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Var */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.V p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.A p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Lyst */
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum p, A arg) {
      R r = leaf(arg);
      for (Term x : p.listterm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons p, A arg) {
      R r = leaf(arg);
      for (Term x : p.listterm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.lyst_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV p, A arg) {
      R r = leaf(arg);
      for (Term x : p.listterm_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.var_.accept(this, arg), r, arg);
      return r;
    }


}
