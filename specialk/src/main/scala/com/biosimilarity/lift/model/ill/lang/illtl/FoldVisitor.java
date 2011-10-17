package com.biosimilarity.seleKt.model.ill.lang.illtl;

import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* RLLExpr */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_.accept(this, arg), r, arg);
      for (RLLExpr x : p.listrllexpr_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_1.accept(this, arg), r, arg);
      r = combine(p.rllexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_1.accept(this, arg), r, arg);
      r = combine(p.rllexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction p, A arg) {
      R r = leaf(arg);
      for (FormalExpr x : p.listformalexpr_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      r = combine(p.rllexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_1.accept(this, arg), r, arg);
      r = combine(p.rllptrn_.accept(this, arg), r, arg);
      r = combine(p.rllexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_1.accept(this, arg), r, arg);
      r = combine(p.rllleftptrn_.accept(this, arg), r, arg);
      r = combine(p.rllexpr_2.accept(this, arg), r, arg);
      r = combine(p.rllrightptrn_.accept(this, arg), r, arg);
      r = combine(p.rllexpr_3.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value p, A arg) {
      R r = leaf(arg);
      r = combine(p.valueexpr_.accept(this, arg), r, arg);
      return r;
    }

/* RLLLeftPtrn */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_.accept(this, arg), r, arg);
      return r;
    }

/* RLLRightPtrn */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_.accept(this, arg), r, arg);
      return r;
    }

/* RLLPtrn */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_1.accept(this, arg), r, arg);
      r = combine(p.formalexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_1.accept(this, arg), r, arg);
      r = combine(p.formalexpr_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction p, A arg) {
      R r = leaf(arg);
      r = combine(p.formalexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* FormalExpr */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription p, A arg) {
      R r = leaf(arg);
      r = combine(p.rllexpr_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* ValueExpr */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
