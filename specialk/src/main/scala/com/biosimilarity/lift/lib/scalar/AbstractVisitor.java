package com.biosimilarity.lift.lib.scalar;
import com.biosimilarity.lift.lib.scalar.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Program */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Progression p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Completion p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Binding p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.Program p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Expression */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Calculation p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Embedding p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.Expression p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ArithmeticExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Summation p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Multiplication p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Negation p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Function p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* LambdaExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Application p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Abstraction p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Mention p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Value p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Association p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* VariableExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Atom p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Transcription p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ValueExpr */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Listed p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Quantity p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Quality p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Utterance p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Numeric */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Measure p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Count p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.Numeric p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Logical */
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Verity p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Absurdity p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.lift.lib.scalar.Absyn.Logical p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
