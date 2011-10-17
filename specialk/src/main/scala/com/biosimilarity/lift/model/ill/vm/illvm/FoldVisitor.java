package com.biosimilarity.seleKt.model.ill.vm.illvm;

import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* State */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE p, A arg) {
      R r = leaf(arg);
      r = combine(p.stack_.accept(this, arg), r, arg);
      r = combine(p.env_.accept(this, arg), r, arg);
      r = combine(p.illcode_.accept(this, arg), r, arg);
      r = combine(p.dump_.accept(this, arg), r, arg);
      return r;
    }

/* Stack */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK p, A arg) {
      R r = leaf(arg);
      for (EnvOrVal x : p.listenvorval_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* Dump */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP p, A arg) {
      R r = leaf(arg);
      for (Frame x : p.listframe_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* EnvOrVal */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV p, A arg) {
      R r = leaf(arg);
      r = combine(p.env_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }

/* Frame */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME p, A arg) {
      R r = leaf(arg);
      r = combine(p.stack_.accept(this, arg), r, arg);
      r = combine(p.env_.accept(this, arg), r, arg);
      r = combine(p.illcode_.accept(this, arg), r, arg);
      return r;
    }

/* ILLCode */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ p, A arg) {
      R r = leaf(arg);
      for (Instruction x : p.listinstruction_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }

/* Instruction */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_1.accept(this, arg), r, arg);
      r = combine(p.illcode_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_1.accept(this, arg), r, arg);
      r = combine(p.illcode_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Value */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_1.accept(this, arg), r, arg);
      r = combine(p.value_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_.accept(this, arg), r, arg);
      r = combine(p.env_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_1.accept(this, arg), r, arg);
      r = combine(p.illcode_2.accept(this, arg), r, arg);
      r = combine(p.env_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV p, A arg) {
      R r = leaf(arg);
      r = combine(p.illcode_.accept(this, arg), r, arg);
      r = combine(p.env_.accept(this, arg), r, arg);
      return r;
    }

/* Env */
    public R visit(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT p, A arg) {
      R r = leaf(arg);
      for (Value x : p.listvalue_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }


}
