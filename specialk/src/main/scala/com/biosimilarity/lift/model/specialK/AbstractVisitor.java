package com.biosimilarity.lift.model.specialK;
import com.biosimilarity.lift.model.specialK.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Agent */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Composition p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Superposition p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Replication p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Ingestion p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Excretion p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Agent p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* GuardedAgent */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Injection p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Abstraction */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Applicant p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Abstraction p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Concretion */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Applicand p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Concretion p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Pattern */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Element p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Variable p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Literal p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Pattern p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Symbol */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Tag p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Symbol p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Variation */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Atomic p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Transcription p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Variation p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Information */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Indirection p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Reflection p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Information p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Value */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.StringLiteral p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Value p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Duality */
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Verity p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Absurdity p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.model.specialK.Absyn.Duality p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
