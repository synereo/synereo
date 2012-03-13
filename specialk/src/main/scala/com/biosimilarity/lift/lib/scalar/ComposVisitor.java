package com.biosimilarity.lift.lib.scalar;
import com.biosimilarity.lift.lib.scalar.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  com.biosimilarity.lift.lib.scalar.Absyn.Program.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Program,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Expression.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Expression,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Numeric.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Numeric,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Logical.Visitor<com.biosimilarity.lift.lib.scalar.Absyn.Logical,A>
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
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Calculation p, A arg)
    {
      ArithmeticExpr arithmeticexpr_ = p.arithmeticexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Calculation(arithmeticexpr_);
    }
    public Expression visit(com.biosimilarity.lift.lib.scalar.Absyn.Embedding p, A arg)
    {
      Program program_ = p.program_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Embedding(program_);
    }

/* ArithmeticExpr */
    public ArithmeticExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Summation p, A arg)
    {
      ArithmeticExpr arithmeticexpr_1 = p.arithmeticexpr_1.accept(this, arg);
      ArithmeticExpr arithmeticexpr_2 = p.arithmeticexpr_2.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Summation(arithmeticexpr_1, arithmeticexpr_2);
    }
    public ArithmeticExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Multiplication p, A arg)
    {
      ArithmeticExpr arithmeticexpr_1 = p.arithmeticexpr_1.accept(this, arg);
      ArithmeticExpr arithmeticexpr_2 = p.arithmeticexpr_2.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Multiplication(arithmeticexpr_1, arithmeticexpr_2);
    }
    public ArithmeticExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Negation p, A arg)
    {
      ArithmeticExpr arithmeticexpr_ = p.arithmeticexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Negation(arithmeticexpr_);
    }
    public ArithmeticExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Function p, A arg)
    {
      LambdaExpr lambdaexpr_ = p.lambdaexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Function(lambdaexpr_);
    }

/* LambdaExpr */
    public LambdaExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Application p, A arg)
    {
      LambdaExpr lambdaexpr_ = p.lambdaexpr_.accept(this, arg);
      ListLambdaExpr listlambdaexpr_ = new ListLambdaExpr();
      for (LambdaExpr x : p.listlambdaexpr_) {
        listlambdaexpr_.add(x.accept(this,arg));
      }

      return new com.biosimilarity.lift.lib.scalar.Absyn.Application(lambdaexpr_, listlambdaexpr_);
    }
    public LambdaExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Abstraction p, A arg)
    {
      ListVariableExpr listvariableexpr_ = new ListVariableExpr();
      for (VariableExpr x : p.listvariableexpr_) {
        listvariableexpr_.add(x.accept(this,arg));
      }
      Program program_ = p.program_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Abstraction(listvariableexpr_, program_);
    }
    public LambdaExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Mention p, A arg)
    {
      VariableExpr variableexpr_ = p.variableexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Mention(variableexpr_);
    }
    public LambdaExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Value p, A arg)
    {
      ValueExpr valueexpr_ = p.valueexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Value(valueexpr_);
    }
    public LambdaExpr visit(com.biosimilarity.lift.lib.scalar.Absyn.Association p, A arg)
    {
      ArithmeticExpr arithmeticexpr_ = p.arithmeticexpr_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Association(arithmeticexpr_);
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
      Logical logical_ = p.logical_.accept(this, arg);

      return new com.biosimilarity.lift.lib.scalar.Absyn.Quality(logical_);
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

/* Logical */
    public Logical visit(com.biosimilarity.lift.lib.scalar.Absyn.Verity p, A arg)
    {

      return new com.biosimilarity.lift.lib.scalar.Absyn.Verity();
    }
    public Logical visit(com.biosimilarity.lift.lib.scalar.Absyn.Absurdity p, A arg)
    {

      return new com.biosimilarity.lift.lib.scalar.Absyn.Absurdity();
    }

}