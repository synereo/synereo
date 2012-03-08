package com.biosimilarity.lift.lib.scalar;
import com.biosimilarity.lift.lib.scalar.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.lib.scalar.Absyn.Program.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Program,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Expression.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Expression,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Numeric.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Numeric,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Bool.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Bool,A>
{
/* Program */
    public Program visit(com.biosimilarity.lift.lib.scalar.Absyn.Progression p, A arg)
    {
      Expression expression_ = p.expression_.accept(this, arg);
      Program program_ = p.program_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Progression(expression_, program_);
    }
    public Program visit(com.biosimilarity.lift.lib.scalar.Absyn.Completion p, A arg)
    {

      return new com.biosimilarity.lift.lib.scalar.Absyn.Completion();
    }
    public Program visit(com.biosimilarity.lift.lib.scalar.Absyn.Binding p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);
      Expression expression_ = p.expression_.accept(this, arg);
      Program program_ = p.program_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Binding(variableexpr_, expression_, program_);
    }

/* Expression */
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Application p, A arg)
    {
      Expression expression_ = p.expression_.accept(this, arg);
      ListExpression listexpression_ = new ListExpression();
      for (Expression x : p.listexpression_) {
        listexpression_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.scalar.Absyn.Application(expression_, listexpression_);
    }
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Abstraction p, A arg)
    {
      ListVariableExpr listvariableexpr_ = new ListVariableExpr();
      for (VariableExpr x : p.listvariableexpr_) {
        listvariableexpr_.add(x.accept(this,arg));
      }
      Program program_ = p.program_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Abstraction(listvariableexpr_, program_);
    }
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Mention p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Mention(variableexpr_);
    }
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Value p, A arg)
    {
      ValueExpr valueexpr_ = p.valueexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Value(valueexpr_);
    }
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Embedding p, A arg)
    {
      Program program_ = p.program_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Embedding(program_);
    }

/* VariableExpr */
    public VariableExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Atom p, A arg)
    {
      String ident_ = p.ident_;

      return new com.biosimilarity.lift.lib.scalar.Absyn.Atom(ident_);
    }
    public VariableExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Transcription p, A arg)
    {
      Expression expression_ = p.expression_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Transcription(expression_);
    }

/* ValueExpr */
    public ValueExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Listed p, A arg)
    {
      ListExpression listexpression_ = new ListExpression();
      for (Expression x : p.listexpression_) {
        listexpression_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.scalar.Absyn.Listed(listexpression_);
    }
    public ValueExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Quantity p, A arg)
    {
      Numeric numeric_ = p.numeric_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Quantity(numeric_);
    }
    public ValueExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Quality p, A arg)
    {
      Bool bool_ = p.bool_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Quality(bool_);
    }
    public ValueExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Utterance p, A arg)
    {
      String string_ = p.string_;

      return new com.biosimilarity.lift.lib.scalar.Absyn.Utterance(string_);
    }

/* Numeric */
    public Numeric visit(com.biosimilarity.lift.lib.scalar.Absyn.Measure p, A arg)
    {
      Double double_ = p.double_;

      return new com.biosimilarity.lift.lib.scalar.Absyn.Measure(double_);
    }
    public Numeric visit(com.biosimilarity.lift.lib.scalar.Absyn.Count p, A arg)
    {
      Integer integer_ = p.integer_;

      return new com.biosimilarity.lift.lib.scalar.Absyn.Count(integer_);
    }

/* Bool */
    public Bool visit(com.biosimilarity.lift.lib.scalar.Absyn.Verity p, A arg)
    {

      return new com.biosimilarity.lift.lib.scalar.Absyn.Verity();
    }
    public Bool visit(com.biosimilarity.lift.lib.scalar.Absyn.Absurdity p, A arg)
    {

      return new com.biosimilarity.lift.lib.scalar.Absyn.Absurdity();
    }

}