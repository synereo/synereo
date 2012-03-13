package com.biosimilarity.lift.lib.scalar;

import com.biosimilarity.lift.lib.scalar.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  com.biosimilarity.lift.lib.scalar.Absyn.Program.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Expression.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Numeric.Visitor<R,A>,
  com.biosimilarity.lift.lib.scalar.Absyn.Logical.Visitor<R,A>
{}
