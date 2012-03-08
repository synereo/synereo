package com.biosimilarity.lift.lib.scalar;
import com.biosimilarity.lift.lib.scalar.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class ProgramVisitor<R,A> implements Program.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Progression p, A arg)
    {
      /* Code For Progression Goes Here */

      p.expression_.accept(new ExpressionVisitor<R,A>(), arg);
      p.program_.accept(new ProgramVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Completion p, A arg)
    {
      /* Code For Completion Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Binding p, A arg)
    {
      /* Code For Binding Goes Here */

      p.variableexpr_.accept(new VariableExprVisitor<R,A>(), arg);
      p.expression_.accept(new ExpressionVisitor<R,A>(), arg);
      p.program_.accept(new ProgramVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ExpressionVisitor<R,A> implements Expression.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Application p, A arg)
    {
      /* Code For Application Goes Here */

      p.expression_.accept(new ExpressionVisitor<R,A>(), arg);
      for (Expression x : p.listexpression_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Abstraction p, A arg)
    {
      /* Code For Abstraction Goes Here */

      for (VariableExpr x : p.listvariableexpr_) {
      }
      p.program_.accept(new ProgramVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Mention p, A arg)
    {
      /* Code For Mention Goes Here */

      p.variableexpr_.accept(new VariableExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Value p, A arg)
    {
      /* Code For Value Goes Here */

      p.valueexpr_.accept(new ValueExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Embedding p, A arg)
    {
      /* Code For Embedding Goes Here */

      p.program_.accept(new ProgramVisitor<R,A>(), arg);

      return null;
    }

  }
  public class VariableExprVisitor<R,A> implements VariableExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Atom p, A arg)
    {
      /* Code For Atom Goes Here */

      //p.ident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Transcription p, A arg)
    {
      /* Code For Transcription Goes Here */

      p.expression_.accept(new ExpressionVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ValueExprVisitor<R,A> implements ValueExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Listed p, A arg)
    {
      /* Code For Listed Goes Here */

      for (Expression x : p.listexpression_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Quantity p, A arg)
    {
      /* Code For Quantity Goes Here */

      p.numeric_.accept(new NumericVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Quality p, A arg)
    {
      /* Code For Quality Goes Here */

      p.bool_.accept(new BoolVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Utterance p, A arg)
    {
      /* Code For Utterance Goes Here */

      //p.string_;

      return null;
    }

  }
  public class NumericVisitor<R,A> implements Numeric.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Measure p, A arg)
    {
      /* Code For Measure Goes Here */

      //p.double_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Count p, A arg)
    {
      /* Code For Count Goes Here */

      //p.integer_;

      return null;
    }

  }
  public class BoolVisitor<R,A> implements Bool.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Verity p, A arg)
    {
      /* Code For Verity Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.scalar.Absyn.Absurdity p, A arg)
    {
      /* Code For Absurdity Goes Here */


      return null;
    }

  }
}