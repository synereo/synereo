package com.biosimilarity.lift.model.specialK;

import com.biosimilarity.lift.model.specialK.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Agent */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Composition p, A arg) {
      R r = leaf(arg);
      for (Agent x : p.listagent_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Superposition p, A arg) {
      R r = leaf(arg);
      for (GuardedAgent x : p.listguardedagent_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Replication p, A arg) {
      R r = leaf(arg);
      r = combine(p.variation_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Ingestion p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.abstraction_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Excretion p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.concretion_.accept(this, arg), r, arg);
      return r;
    }

/* GuardedAgent */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Injection p, A arg) {
      R r = leaf(arg);
      r = combine(p.pattern_.accept(this, arg), r, arg);
      r = combine(p.agent_.accept(this, arg), r, arg);
      return r;
    }

/* Abstraction */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Applicant p, A arg) {
      R r = leaf(arg);
      r = combine(p.variation_.accept(this, arg), r, arg);
      r = combine(p.agent_.accept(this, arg), r, arg);
      return r;
    }

/* Concretion */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Applicand p, A arg) {
      R r = leaf(arg);
      r = combine(p.information_.accept(this, arg), r, arg);
      r = combine(p.agent_.accept(this, arg), r, arg);
      return r;
    }

/* Pattern */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Element p, A arg) {
      R r = leaf(arg);
      r = combine(p.symbol_.accept(this, arg), r, arg);
      for (Pattern x : p.listpattern_) {
        r = combine(x.accept(this,arg), r, arg);
      }
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Variable p, A arg) {
      R r = leaf(arg);
      r = combine(p.variation_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Literal p, A arg) {
      R r = leaf(arg);
      r = combine(p.value_.accept(this, arg), r, arg);
      return r;
    }

/* Symbol */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Tag p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Variation */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Atomic p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Transcription p, A arg) {
      R r = leaf(arg);
      r = combine(p.agent_.accept(this, arg), r, arg);
      return r;
    }

/* Information */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Indirection p, A arg) {
      R r = leaf(arg);
      r = combine(p.variation_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Reflection p, A arg) {
      R r = leaf(arg);
      r = combine(p.agent_.accept(this, arg), r, arg);
      return r;
    }

/* Value */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral p, A arg) {
      R r = leaf(arg);
      r = combine(p.duality_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.StringLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Duality */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Verity p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Absurdity p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
