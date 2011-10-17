package com.biosimilarity.seleKt.model.ill.lang.illtl;
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* RLLExpr */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RLLLeftPtrn */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RLLRightPtrn */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* RLLPtrn */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* FormalExpr */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ValueExpr */
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
