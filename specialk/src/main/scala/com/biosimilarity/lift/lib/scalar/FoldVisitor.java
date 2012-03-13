package com.biosimilarity.lift.lib.scalar;

import com.biosimilarity.lift.lib.scalar.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Program */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Progression p, A arg) {
      R r = leaf(arg);
      r = combine(p.expression_.accept(this, arg), r, arg);
      r = combine(p.program_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Completion p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Binding p, A arg) {
      R r = leaf(arg);
      r = combine(p.variableexpr_.accept(this, arg), r, arg);
      r = combine(p.expression_.accept(this, arg), r, arg);
      r = combine(p.program_.accept(this, arg), r, arg);
      return r;
    }

/* Expression */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Calculation p, A arg) {
      R r = leaf(arg);
      r = combine(p.arithmeticexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Embedding p, A arg) {
      R r = leaf(arg);
      r = combine(p.program_.accept(this, arg), r, arg);
      return r;
    }

/* ArithmeticExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Summation p, A arg) {
      R r = leaf(arg);
      r = combine(p.arithmeticexpr_1.accept(this, arg), r, arg);
      r = combine(p.arithmeticexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Multiplication p, A arg) {
      R r = leaf(arg);
      r = combine(p.arithmeticexpr_1.accept(this, arg), r, arg);
      r = combine(p.arithmeticexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Negation p, A arg) {
      R r = leaf(arg);
      r = combine(p.arithmeticexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Function p, A arg) {
      R r = leaf(arg);
      r = combine(p.lambdaexpr_.accept(this, arg), r, arg);
      return r;
    }

/* LambdaExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Application p, A arg) {
      R r = leaf(arg);
      r = combine(p.lambdaexpr_.accept(this, arg), r, arg);
      for (LambdaExpr x : p.listlambdaexpr_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Abstraction p, A arg) {
      R r = leaf(arg);
      for (VariableExpr x : p.listvariableexpr_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.program_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Mention p, A arg) {
      R r = leaf(arg);
      r = combine(p.variableexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Value p, A arg) {
      R r = leaf(arg);
      r = combine(p.valueexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Association p, A arg) {
      R r = leaf(arg);
      r = combine(p.arithmeticexpr_.accept(this, arg), r, arg);
      return r;
    }

/* VariableExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Atom p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Transcription p, A arg) {
      R r = leaf(arg);
      r = combine(p.expression_.accept(this, arg), r, arg);
      return r;
    }

/* ValueExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Listed p, A arg) {
      R r = leaf(arg);
      for (Expression x : p.listexpression_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Quantity p, A arg) {
      R r = leaf(arg);
      r = combine(p.numeric_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Quality p, A arg) {
      R r = leaf(arg);
      r = combine(p.logical_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Utterance p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Numeric */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Measure p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Count p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Logical */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Verity p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Absurdity p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
