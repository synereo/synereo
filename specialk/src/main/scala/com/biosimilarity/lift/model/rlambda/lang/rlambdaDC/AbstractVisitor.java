package com.biosimilarity.rlambdaDC.lang.rlambdaDC;
import com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Expression */
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Application p, A arg) { return visitDefault(p, arg); }

    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Mention p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Value p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Continuation p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Abstraction p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Expression p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ContinueExpr */
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Prompt p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushPrompt p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Subcontinuation p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushSubCont p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.ContinueExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* VariableExpr */
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Transcription p, A arg) { return visitDefault(p, arg); }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.AtomLiteral p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.VariableExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* ValueExpr */
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Numeric p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.ValueExpr p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
