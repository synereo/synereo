package com.biosimilarity.lift.model.specialK;
import com.biosimilarity.lift.model.specialK.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class AgentVisitor<R,A> implements Agent.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Composition p, A arg)
    {
      /* Code For Composition Goes Here */

      for (Agent x : p.listagent_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Superposition p, A arg)
    {
      /* Code For Superposition Goes Here */

      for (GuardedAgent x : p.listguardedagent_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Replication p, A arg)
    {
      /* Code For Replication Goes Here */

      p.variation_.accept(new VariationVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Ingestion p, A arg)
    {
      /* Code For Ingestion Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.abstraction_.accept(new AbstractionVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Excretion p, A arg)
    {
      /* Code For Excretion Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.concretion_.accept(new ConcretionVisitor<R,A>(), arg);

      return null;
    }

  }
  public class GuardedAgentVisitor<R,A> implements GuardedAgent.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Injection p, A arg)
    {
      /* Code For Injection Goes Here */

      p.pattern_.accept(new PatternVisitor<R,A>(), arg);
      p.agent_.accept(new AgentVisitor<R,A>(), arg);

      return null;
    }

  }
  public class AbstractionVisitor<R,A> implements Abstraction.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Applicant p, A arg)
    {
      /* Code For Applicant Goes Here */

      p.variation_.accept(new VariationVisitor<R,A>(), arg);
      p.agent_.accept(new AgentVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ConcretionVisitor<R,A> implements Concretion.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Applicand p, A arg)
    {
      /* Code For Applicand Goes Here */

      p.information_.accept(new InformationVisitor<R,A>(), arg);
      p.agent_.accept(new AgentVisitor<R,A>(), arg);

      return null;
    }

  }
  public class PatternVisitor<R,A> implements Pattern.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Element p, A arg)
    {
      /* Code For Element Goes Here */

      p.symbol_.accept(new SymbolVisitor<R,A>(), arg);
      for (Pattern x : p.listpattern_) {
      }

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Variable p, A arg)
    {
      /* Code For Variable Goes Here */

      p.variation_.accept(new VariationVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Literal p, A arg)
    {
      /* Code For Literal Goes Here */

      p.value_.accept(new ValueVisitor<R,A>(), arg);

      return null;
    }

  }
  public class SymbolVisitor<R,A> implements Symbol.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Tag p, A arg)
    {
      /* Code For Tag Goes Here */

      //p.lident_;

      return null;
    }

  }
  public class VariationVisitor<R,A> implements Variation.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Atomic p, A arg)
    {
      /* Code For Atomic Goes Here */

      //p.uident_;

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Transcription p, A arg)
    {
      /* Code For Transcription Goes Here */

      p.agent_.accept(new AgentVisitor<R,A>(), arg);

      return null;
    }

  }
  public class InformationVisitor<R,A> implements Information.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Indirection p, A arg)
    {
      /* Code For Indirection Goes Here */

      p.variation_.accept(new VariationVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Reflection p, A arg)
    {
      /* Code For Reflection Goes Here */

      p.agent_.accept(new AgentVisitor<R,A>(), arg);

      return null;
    }

  }
  public class ValueVisitor<R,A> implements Value.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral p, A arg)
    {
      /* Code For BooleanLiteral Goes Here */

      p.duality_.accept(new DualityVisitor<R,A>(), arg);

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.StringLiteral p, A arg)
    {
      /* Code For StringLiteral Goes Here */

      //p.string_;

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral p, A arg)
    {
      /* Code For IntegerLiteral Goes Here */

      //p.integer_;

      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral p, A arg)
    {
      /* Code For DoubleLiteral Goes Here */

      //p.double_;

      return null;
    }

  }
  public class DualityVisitor<R,A> implements Duality.Visitor<R,A>
  {
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Verity p, A arg)
    {
      /* Code For Verity Goes Here */


      return null;
    }
    public R visit(com.biosimilarity.lift.model.specialK.Absyn.Absurdity p, A arg)
    {
      /* Code For Absurdity Goes Here */


      return null;
    }

  }
}