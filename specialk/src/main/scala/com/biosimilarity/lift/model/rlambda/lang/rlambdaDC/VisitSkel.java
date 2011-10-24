package com.biosimilarity.rlambdaDC.lang.rlambdaDC;
import com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class ExpressionVisitor<R,A> implements Expression.Visitor<R,A>
  {
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Application p, A arg)
    {
      /* Code For Application Goes Here */

      p.expression_1.accept(new ExpressionVisitor<R,A>(), arg);
      p.expression_2.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Mention p, A arg)
    {
      /* Code For Mention Goes Here */

      p.variableexpr_.accept(new VariableExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Value p, A arg)
    {
      /* Code For Value Goes Here */

      p.valueexpr_.accept(new ValueExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Continuation p, A arg)
    {
      /* Code For Continuation Goes Here */

      p.continueexpr_.accept(new ContinueExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Abstraction p, A arg)
    {
      /* Code For Abstraction Goes Here */

      for (VariableExpr x : p.listvariableexpr_) {
      }
      p.expression_.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ContinueExprVisitor<R,A> implements ContinueExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Prompt p, A arg)
    {
      /* Code For Prompt Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushPrompt p, A arg)
    {
      /* Code For PushPrompt Goes Here */

      p.expression_1.accept(new ExpressionVisitor<R,A>(), arg);
      p.expression_2.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Subcontinuation p, A arg)
    {
      /* Code For Subcontinuation Goes Here */

      p.expression_1.accept(new ExpressionVisitor<R,A>(), arg);
      p.expression_2.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.PushSubCont p, A arg)
    {
      /* Code For PushSubCont Goes Here */

      p.expression_1.accept(new ExpressionVisitor<R,A>(), arg);
      p.expression_2.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }

  }
  public class VariableExprVisitor<R,A> implements VariableExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Transcription p, A arg)
    {
      /* Code For Transcription Goes Here */

      p.expression_.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.AtomLiteral p, A arg)
    {
      /* Code For AtomLiteral Goes Here */

      //p.ident_;

      return null;
    }

  }
  public class ValueExprVisitor<R,A> implements ValueExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.rlambdaDC.lang.rlambdaDC.Absyn.Numeric p, A arg)
    {
      /* Code For Numeric Goes Here */

      //p.integer_;

      return null;
    }

  }
}