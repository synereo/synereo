package com.biosimilarity.seleKt.model.ill.lang.illtl;
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class RLLExprVisitor<R,A> implements RLLExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application p, A arg)
    {
      /* Code For Application Goes Here */

      p.rllexpr_.accept(new RLLExprVisitor<R,A>(), arg);
      for (RLLExpr x : p.listrllexpr_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation p, A arg)
    {
      /* Code For Separation Goes Here */

      p.rllexpr_1.accept(new RLLExprVisitor<R,A>(), arg);
      p.rllexpr_2.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion p, A arg)
    {
      /* Code For Inclusion Goes Here */

      p.rllexpr_1.accept(new RLLExprVisitor<R,A>(), arg);
      p.rllexpr_2.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction p, A arg)
    {
      /* Code For Abstraction Goes Here */

      for (FormalExpr x : p.listformalexpr_) {
      }
      p.rllexpr_.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft p, A arg)
    {
      /* Code For InjectionLeft Goes Here */

      p.rllexpr_.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight p, A arg)
    {
      /* Code For InjectionRight Goes Here */

      p.rllexpr_.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration p, A arg)
    {
      /* Code For Duration Goes Here */

      p.rllexpr_.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction p, A arg)
    {
      /* Code For Deconstruction Goes Here */

      p.rllexpr_1.accept(new RLLExprVisitor<R,A>(), arg);
      p.rllptrn_.accept(new RLLPtrnVisitor<R,A>(), arg);
      p.rllexpr_2.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection p, A arg)
    {
      /* Code For Selection Goes Here */

      p.rllexpr_1.accept(new RLLExprVisitor<R,A>(), arg);
      p.rllleftptrn_.accept(new RLLLeftPtrnVisitor<R,A>(), arg);
      p.rllexpr_2.accept(new RLLExprVisitor<R,A>(), arg);
      p.rllrightptrn_.accept(new RLLRightPtrnVisitor<R,A>(), arg);
      p.rllexpr_3.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention p, A arg)
    {
      /* Code For Mention Goes Here */

      p.formalexpr_.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value p, A arg)
    {
      /* Code For Value Goes Here */

      p.valueexpr_.accept(new ValueExprVisitor<R,A>(), arg);

      return null;
    }

  }
  public class RLLLeftPtrnVisitor<R,A> implements RLLLeftPtrn.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft p, A arg)
    {
      /* Code For InLeft Goes Here */

      p.formalexpr_.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }

  }
  public class RLLRightPtrnVisitor<R,A> implements RLLRightPtrn.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight p, A arg)
    {
      /* Code For InRight Goes Here */

      p.formalexpr_.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }

  }
  public class RLLPtrnVisitor<R,A> implements RLLPtrn.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn p, A arg)
    {
      /* Code For SeparationPtn Goes Here */

      p.formalexpr_1.accept(new FormalExprVisitor<R,A>(), arg);
      p.formalexpr_2.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn p, A arg)
    {
      /* Code For DuplicationPtn Goes Here */

      p.formalexpr_1.accept(new FormalExprVisitor<R,A>(), arg);
      p.formalexpr_2.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft p, A arg)
    {
      /* Code For InclusionLeft Goes Here */

      p.formalexpr_.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight p, A arg)
    {
      /* Code For InclusionRight Goes Here */

      p.formalexpr_.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction p, A arg)
    {
      /* Code For Extraction Goes Here */

      p.formalexpr_.accept(new FormalExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard p, A arg)
    {
      /* Code For Wildcard Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn p, A arg)
    {
      /* Code For UnitPtn Goes Here */


      return null;
    }

  }
  public class FormalExprVisitor<R,A> implements FormalExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription p, A arg)
    {
      /* Code For Transcription Goes Here */

      p.rllexpr_.accept(new RLLExprVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral p, A arg)
    {
      /* Code For AtomLiteral Goes Here */

      //p.ident_;

      return null;
    }

  }
  public class ValueExprVisitor<R,A> implements ValueExpr.Visitor<R,A>
  {
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral p, A arg)
    {
      /* Code For DecimalLiteral Goes Here */

      //p.double_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral p, A arg)
    {
      /* Code For IntegerLiteral Goes Here */

      //p.integer_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral p, A arg)
    {
      /* Code For StringLiteral Goes Here */

      //p.string_;

      return null;
    }
    public R visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral p, A arg)
    {
      /* Code For UnitLiteral Goes Here */


      return null;
    }

  }
}