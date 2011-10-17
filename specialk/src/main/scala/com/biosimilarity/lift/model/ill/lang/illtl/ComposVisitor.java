package com.biosimilarity.seleKt.model.ill.lang.illtl;
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr.Visitor<com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr,A>,
  com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn.Visitor<com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn,A>,
  com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn.Visitor<com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn,A>,
  com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn.Visitor<com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn,A>,
  com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr.Visitor<com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr,A>,
  com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr.Visitor<com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr,A>
{
/* RLLExpr */
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application p, A arg)
    {
      RLLExpr rllexpr_ = p.rllexpr_.accept(this, arg);
      ListRLLExpr listrllexpr_ = new ListRLLExpr();
      for (RLLExpr x : p.listrllexpr_) {
        listrllexpr_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application(rllexpr_, listrllexpr_);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation p, A arg)
    {
      RLLExpr rllexpr_1 = p.rllexpr_1.accept(this, arg);
      RLLExpr rllexpr_2 = p.rllexpr_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation(rllexpr_1, rllexpr_2);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion p, A arg)
    {
      RLLExpr rllexpr_1 = p.rllexpr_1.accept(this, arg);
      RLLExpr rllexpr_2 = p.rllexpr_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion(rllexpr_1, rllexpr_2);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction p, A arg)
    {
      ListFormalExpr listformalexpr_ = new ListFormalExpr();
      for (FormalExpr x : p.listformalexpr_) {
        listformalexpr_.add(x.accept(this,arg));
      }
      RLLExpr rllexpr_ = p.rllexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction(listformalexpr_, rllexpr_);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft p, A arg)
    {
      RLLExpr rllexpr_ = p.rllexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft(rllexpr_);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight p, A arg)
    {
      RLLExpr rllexpr_ = p.rllexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight(rllexpr_);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration p, A arg)
    {
      RLLExpr rllexpr_ = p.rllexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration(rllexpr_);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction p, A arg)
    {
      RLLExpr rllexpr_1 = p.rllexpr_1.accept(this, arg);
      RLLPtrn rllptrn_ = p.rllptrn_.accept(this, arg);
      RLLExpr rllexpr_2 = p.rllexpr_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction(rllexpr_1, rllptrn_, rllexpr_2);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection p, A arg)
    {
      RLLExpr rllexpr_1 = p.rllexpr_1.accept(this, arg);
      RLLLeftPtrn rllleftptrn_ = p.rllleftptrn_.accept(this, arg);
      RLLExpr rllexpr_2 = p.rllexpr_2.accept(this, arg);
      RLLRightPtrn rllrightptrn_ = p.rllrightptrn_.accept(this, arg);
      RLLExpr rllexpr_3 = p.rllexpr_3.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection(rllexpr_1, rllleftptrn_, rllexpr_2, rllrightptrn_, rllexpr_3);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention p, A arg)
    {
      FormalExpr formalexpr_ = p.formalexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention(formalexpr_);
    }
    public RLLExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value p, A arg)
    {
      ValueExpr valueexpr_ = p.valueexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value(valueexpr_);
    }

/* RLLLeftPtrn */
    public RLLLeftPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft p, A arg)
    {
      FormalExpr formalexpr_ = p.formalexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft(formalexpr_);
    }

/* RLLRightPtrn */
    public RLLRightPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight p, A arg)
    {
      FormalExpr formalexpr_ = p.formalexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight(formalexpr_);
    }

/* RLLPtrn */
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn p, A arg)
    {
      FormalExpr formalexpr_1 = p.formalexpr_1.accept(this, arg);
      FormalExpr formalexpr_2 = p.formalexpr_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn(formalexpr_1, formalexpr_2);
    }
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn p, A arg)
    {
      FormalExpr formalexpr_1 = p.formalexpr_1.accept(this, arg);
      FormalExpr formalexpr_2 = p.formalexpr_2.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn(formalexpr_1, formalexpr_2);
    }
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft p, A arg)
    {
      FormalExpr formalexpr_ = p.formalexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft(formalexpr_);
    }
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight p, A arg)
    {
      FormalExpr formalexpr_ = p.formalexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight(formalexpr_);
    }
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction p, A arg)
    {
      FormalExpr formalexpr_ = p.formalexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction(formalexpr_);
    }
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard p, A arg)
    {

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard();
    }
    public RLLPtrn visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn p, A arg)
    {

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn();
    }

/* FormalExpr */
    public FormalExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription p, A arg)
    {
      RLLExpr rllexpr_ = p.rllexpr_.accept(this, arg);

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription(rllexpr_);
    }
    public FormalExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral p, A arg)
    {
      String ident_ = p.ident_;

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral(ident_);
    }

/* ValueExpr */
    public ValueExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral(double_);
    }
    public ValueExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral(integer_);
    }
    public ValueExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral(string_);
    }
    public ValueExpr visit(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral p, A arg)
    {

      return new com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral();
    }

}