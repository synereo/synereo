package com.biosimilarity.lift.model.specialK;
import com.biosimilarity.lift.model.specialK.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.model.specialK.Absyn.Agent.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Agent,A>,
  com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent.Visitor<com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Abstraction.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Abstraction,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Concretion.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Concretion,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Pattern.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Pattern,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Symbol.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Symbol,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Variation.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Variation,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Information.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Information,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Value.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Value,A>,
  com.biosimilarity.lift.model.specialK.Absyn.Duality.Visitor<com.biosimilarity.lift.model.specialK.Absyn.Duality,A>
{
/* Agent */
    public Agent visit(com.biosimilarity.lift.model.specialK.Absyn.Composition p, A arg)
    {
      ListAgent listagent_ = new ListAgent();
      for (Agent x : p.listagent_) {
        listagent_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.model.specialK.Absyn.Composition(listagent_);
    }
    public Agent visit(com.biosimilarity.lift.model.specialK.Absyn.Superposition p, A arg)
    {
      ListGuardedAgent listguardedagent_ = new ListGuardedAgent();
      for (GuardedAgent x : p.listguardedagent_) {
        listguardedagent_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.model.specialK.Absyn.Superposition(listguardedagent_);
    }
    public Agent visit(com.biosimilarity.lift.model.specialK.Absyn.Replication p, A arg)
    {
      Variation variation_ = p.variation_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Replication(variation_);
    }
    public Agent visit(com.biosimilarity.lift.model.specialK.Absyn.Ingestion p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Abstraction abstraction_ = p.abstraction_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Ingestion(pattern_, abstraction_);
    }
    public Agent visit(com.biosimilarity.lift.model.specialK.Absyn.Excretion p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Concretion concretion_ = p.concretion_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Excretion(pattern_, concretion_);
    }

/* GuardedAgent */
    public GuardedAgent visit(com.biosimilarity.lift.model.specialK.Absyn.Injection p, A arg)
    {
      Pattern pattern_ = p.pattern_.accept(this, arg);
      Agent agent_ = p.agent_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Injection(pattern_, agent_);
    }

/* Abstraction */
    public Abstraction visit(com.biosimilarity.lift.model.specialK.Absyn.Applicant p, A arg)
    {
      Variation variation_ = p.variation_.accept(this, arg);
      Agent agent_ = p.agent_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Applicant(variation_, agent_);
    }

/* Concretion */
    public Concretion visit(com.biosimilarity.lift.model.specialK.Absyn.Applicand p, A arg)
    {
      Information information_ = p.information_.accept(this, arg);
      Agent agent_ = p.agent_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Applicand(information_, agent_);
    }

/* Pattern */
    public Pattern visit(com.biosimilarity.lift.model.specialK.Absyn.Element p, A arg)
    {
      Symbol symbol_ = p.symbol_.accept(this, arg);
      ListPattern listpattern_ = new ListPattern();
      for (Pattern x : p.listpattern_) {
        listpattern_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.model.specialK.Absyn.Element(symbol_, listpattern_);
    }
    public Pattern visit(com.biosimilarity.lift.model.specialK.Absyn.Variable p, A arg)
    {
      Variation variation_ = p.variation_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Variable(variation_);
    }
    public Pattern visit(com.biosimilarity.lift.model.specialK.Absyn.Literal p, A arg)
    {
      Value value_ = p.value_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Literal(value_);
    }

/* Symbol */
    public Symbol visit(com.biosimilarity.lift.model.specialK.Absyn.Tag p, A arg)
    {
      String lident_ = p.lident_;

      return new com.biosimilarity.lift.model.specialK.Absyn.Tag(lident_);
    }

/* Variation */
    public Variation visit(com.biosimilarity.lift.model.specialK.Absyn.Atomic p, A arg)
    {
      String uident_ = p.uident_;

      return new com.biosimilarity.lift.model.specialK.Absyn.Atomic(uident_);
    }
    public Variation visit(com.biosimilarity.lift.model.specialK.Absyn.Transcription p, A arg)
    {
      Agent agent_ = p.agent_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Transcription(agent_);
    }

/* Information */
    public Information visit(com.biosimilarity.lift.model.specialK.Absyn.Indirection p, A arg)
    {
      Variation variation_ = p.variation_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Indirection(variation_);
    }
    public Information visit(com.biosimilarity.lift.model.specialK.Absyn.Reflection p, A arg)
    {
      Agent agent_ = p.agent_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.Reflection(agent_);
    }

/* Value */
    public Value visit(com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral p, A arg)
    {
      Duality duality_ = p.duality_.accept(this, arg);

      return new com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral(duality_);
    }
    public Value visit(com.biosimilarity.lift.model.specialK.Absyn.StringLiteral p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.model.specialK.Absyn.StringLiteral(string_);
    }
    public Value visit(com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral(integer_);
    }
    public Value visit(com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral(double_);
    }

/* Duality */
    public Duality visit(com.biosimilarity.lift.model.specialK.Absyn.Verity p, A arg)
    {

      return new com.biosimilarity.lift.model.specialK.Absyn.Verity();
    }
    public Duality visit(com.biosimilarity.lift.model.specialK.Absyn.Absurdity p, A arg)
    {

      return new com.biosimilarity.lift.model.specialK.Absyn.Absurdity();
    }

}