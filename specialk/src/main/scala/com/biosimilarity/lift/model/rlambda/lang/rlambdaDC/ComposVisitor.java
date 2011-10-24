package com.biosimilarity.rlambdaDC.lang.rlambdaDC;
import com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Expression.Visitor<com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Expression,A>,
  com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.ContinueExpr.Visitor<com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.ContinueExpr,A>,
  com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.VariableExpr.Visitor<com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.VariableExpr,A>,
  com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.ValueExpr.Visitor<com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.ValueExpr,A>
{
/* Expression */
    public Expression visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Application p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Application(expression_1, expression_2);
    }
    public Expression visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Mention p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Mention(variableexpr_);
    }
    public Expression visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Value p, A arg)
    {
      ValueExpr valueexpr_ = p.valueexpr_.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Value(valueexpr_);
    }
    public Expression visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Continuation p, A arg)
    {
      ContinueExpr continueexpr_ = p.continueexpr_.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Continuation(continueexpr_);
    }
    public Expression visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Abstraction p, A arg)
    {
      ListVariableExpr listvariableexpr_ = new ListVariableExpr();
      for (VariableExpr x : p.listvariableexpr_) {
        listvariableexpr_.add(x.accept(this,arg));
      }
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Abstraction(listvariableexpr_, expression_);
    }

/* ContinueExpr */
    public ContinueExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Prompt p, A arg)
    {

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Prompt();
    }
    public ContinueExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushPrompt p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushPrompt(expression_1, expression_2);
    }
    public ContinueExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Subcontinuation p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Subcontinuation(expression_1, expression_2);
    }
    public ContinueExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushSubCont p, A arg)
    {
      Expression expression_1 = p.expression_1.accept(this, arg);
      Expression expression_2 = p.expression_2.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushSubCont(expression_1, expression_2);
    }

/* VariableExpr */
    public VariableExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Transcription p, A arg)
    {
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Transcription(expression_);
    }
    public VariableExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.AtomLiteral p, A arg)
    {
      String ident_ = p.ident_;

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.AtomLiteral(ident_);
    }

/* ValueExpr */
    public ValueExpr visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Numeric p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Numeric(integer_);
    }

}