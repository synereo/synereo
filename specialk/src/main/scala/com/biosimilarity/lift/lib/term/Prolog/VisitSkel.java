package com.biosimilarity.lift.lib.term.Prolog;
import com.biosimilarity.lift.lib.term.Prolog.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class PredicateVisitor<R,A> implements Predicate.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.APred p, A arg)
    {
      /* Code For APred Goes Here */

      p.atom_.accept(new AtomVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred p, A arg)
    {
      /* Code For CPred Goes Here */

      p.functor_.accept(new FunctorVisitor<R,A>(), arg);
      for (Term x : p.listterm_) {
      }

      return null;
    }

  }
  public class TermVisitor<R,A> implements Term.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom p, A arg)
    {
      /* Code For TAtom Goes Here */

      p.atom_.accept(new AtomVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT p, A arg)
    {
      /* Code For VarT Goes Here */

      p.var_.accept(new VarVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex p, A arg)
    {
      /* Code For Complex Goes Here */

      p.functor_.accept(new FunctorVisitor<R,A>(), arg);
      for (Term x : p.listterm_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.TList p, A arg)
    {
      /* Code For TList Goes Here */

      p.lyst_.accept(new LystVisitor<R,A>(), arg);

      return null;
    }

  }
  public class AtomVisitor<R,A> implements Atom.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm p, A arg)
    {
      /* Code For Atm Goes Here */

      //p.lident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm p, A arg)
    {
      /* Code For EAtm Goes Here */

      //p.ident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm p, A arg)
    {
      /* Code For BAtm Goes Here */

      p.boole_.accept(new BooleVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm p, A arg)
    {
      /* Code For StrAtm Goes Here */

      //p.string_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm p, A arg)
    {
      /* Code For IntAtm Goes Here */

      //p.integer_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm p, A arg)
    {
      /* Code For FltAtm Goes Here */

      //p.double_;

      return null;
    }

  }
  public class FunctorVisitor<R,A> implements Functor.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm p, A arg)
    {
      /* Code For FAtm Goes Here */

      //p.lident_;

      return null;
    }

  }
  public class BooleVisitor<R,A> implements Boole.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity p, A arg)
    {
      /* Code For Verity Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity p, A arg)
    {
      /* Code For Absurdity Goes Here */


      return null;
    }

  }
  public class VarVisitor<R,A> implements Var.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.V p, A arg)
    {
      /* Code For V Goes Here */

      //p.uident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.A p, A arg)
    {
      /* Code For A Goes Here */

      //p.wild_;

      return null;
    }

  }
  public class LystVisitor<R,A> implements Lyst.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty p, A arg)
    {
      /* Code For Empty Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum p, A arg)
    {
      /* Code For Enum Goes Here */

      for (Term x : p.listterm_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons p, A arg)
    {
      /* Code For Cons Goes Here */

      for (Term x : p.listterm_) {
      }
      p.lyst_.accept(new LystVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV p, A arg)
    {
      /* Code For ConsV Goes Here */

      for (Term x : p.listterm_) {
      }
      p.var_.accept(new VarVisitor<R,A>(), arg);

      return null;
    }

  }
}