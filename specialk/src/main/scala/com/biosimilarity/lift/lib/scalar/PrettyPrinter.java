package com.biosimilarity.lift.lib.scalar;
import com.biosimilarity.lift.lib.scalar.Absyn.*;

public class PrettyPrinter
{
  //For certain applications increasing the initial size of the buffer may improve performance.
  private static final int INITIAL_BUFFER_SIZE = 128;
  //You may wish to change the parentheses used in precedence.
  private static final String _L_PAREN = new String("(");
  private static final String _R_PAREN = new String(")");
  //You may wish to change render
  private static void render(String s)
  {
    if (s.equals("{"))
    {
       buf_.append("\n");
       indent();
       buf_.append(s);
       _n_ = _n_ + 2;
       buf_.append("\n");
       indent();
    }
    else if (s.equals("(") || s.equals("["))
       buf_.append(s);
    else if (s.equals(")") || s.equals("]"))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals("}"))
    {
       _n_ = _n_ - 2;
       backup();
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals(","))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals(";"))
    {
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals("")) return;
    else
    {
       buf_.append(s);
       buf_.append(" ");
    }
  }


  //  print and show methods are defined for each category.
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.Program foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.Program foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.Expression foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.Expression foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.Numeric foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.Numeric foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.Logical foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.Logical foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.ListVariableExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.ListVariableExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.ListExpression foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.ListExpression foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.scalar.Absyn.ListLambdaExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.scalar.Absyn.ListLambdaExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.Program foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Progression)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Progression _progression = (com.biosimilarity.lift.lib.scalar.Absyn.Progression) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_progression.expression_, 0);
       render(";");
       pp(_progression.program_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Completion)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Completion _completion = (com.biosimilarity.lift.lib.scalar.Absyn.Completion) foo;
       if (_i_ > 0) render(_L_PAREN);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Binding)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Binding _binding = (com.biosimilarity.lift.lib.scalar.Absyn.Binding) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("val");
       pp(_binding.variableexpr_, 0);
       render("=");
       pp(_binding.expression_, 0);
       render(";");
       pp(_binding.program_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.Expression foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Calculation)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Calculation _calculation = (com.biosimilarity.lift.lib.scalar.Absyn.Calculation) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_calculation.arithmeticexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Embedding)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Embedding _embedding = (com.biosimilarity.lift.lib.scalar.Absyn.Embedding) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_embedding.program_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Summation)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Summation _summation = (com.biosimilarity.lift.lib.scalar.Absyn.Summation) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_summation.arithmeticexpr_1, 0);
       render("+");
       pp(_summation.arithmeticexpr_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Multiplication)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Multiplication _multiplication = (com.biosimilarity.lift.lib.scalar.Absyn.Multiplication) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_multiplication.arithmeticexpr_1, 1);
       render("*");
       pp(_multiplication.arithmeticexpr_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Negation)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Negation _negation = (com.biosimilarity.lift.lib.scalar.Absyn.Negation) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("-");
       pp(_negation.arithmeticexpr_, 2);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Function)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Function _function = (com.biosimilarity.lift.lib.scalar.Absyn.Function) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_function.lambdaexpr_, 0);
       if (_i_ > 2) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Application)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Application _application = (com.biosimilarity.lift.lib.scalar.Absyn.Application) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_application.lambdaexpr_, 1);
       render("(");
       pp(_application.listlambdaexpr_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Abstraction)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Abstraction _abstraction = (com.biosimilarity.lift.lib.scalar.Absyn.Abstraction) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("(");
       pp(_abstraction.listvariableexpr_, 0);
       render(")");
       render("=>");
       render("{");
       pp(_abstraction.program_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Mention)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Mention _mention = (com.biosimilarity.lift.lib.scalar.Absyn.Mention) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_mention.variableexpr_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Value)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Value _value = (com.biosimilarity.lift.lib.scalar.Absyn.Value) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_value.valueexpr_, 0);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Association)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Association _association = (com.biosimilarity.lift.lib.scalar.Absyn.Association) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("{");
       pp(_association.arithmeticexpr_, 0);
       render("}");
       if (_i_ > 1) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Atom)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Atom _atom = (com.biosimilarity.lift.lib.scalar.Absyn.Atom) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atom.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Transcription)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Transcription _transcription = (com.biosimilarity.lift.lib.scalar.Absyn.Transcription) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       render("<");
       pp(_transcription.expression_, 0);
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Listed)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Listed _listed = (com.biosimilarity.lift.lib.scalar.Absyn.Listed) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_listed.listexpression_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Quantity)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Quantity _quantity = (com.biosimilarity.lift.lib.scalar.Absyn.Quantity) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_quantity.numeric_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Quality)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Quality _quality = (com.biosimilarity.lift.lib.scalar.Absyn.Quality) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_quality.logical_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Utterance)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Utterance _utterance = (com.biosimilarity.lift.lib.scalar.Absyn.Utterance) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_utterance.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.Numeric foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Measure)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Measure _measure = (com.biosimilarity.lift.lib.scalar.Absyn.Measure) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_measure.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Count)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Count _count = (com.biosimilarity.lift.lib.scalar.Absyn.Count) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_count.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.Logical foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Verity)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Verity _verity = (com.biosimilarity.lift.lib.scalar.Absyn.Verity) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("true");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Absurdity)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Absurdity _absurdity = (com.biosimilarity.lift.lib.scalar.Absyn.Absurdity) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("false");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.ListVariableExpr foo, int _i_)
  {
     for (java.util.Iterator<VariableExpr> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.ListExpression foo, int _i_)
  {
     for (java.util.Iterator<Expression> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.lib.scalar.Absyn.ListLambdaExpr foo, int _i_)
  {
     for (java.util.Iterator<LambdaExpr> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.Program foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Progression)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Progression _progression = (com.biosimilarity.lift.lib.scalar.Absyn.Progression) foo;
       render("(");
       render("Progression");
       sh(_progression.expression_);
       sh(_progression.program_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Completion)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Completion _completion = (com.biosimilarity.lift.lib.scalar.Absyn.Completion) foo;
       render("Completion");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Binding)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Binding _binding = (com.biosimilarity.lift.lib.scalar.Absyn.Binding) foo;
       render("(");
       render("Binding");
       sh(_binding.variableexpr_);
       sh(_binding.expression_);
       sh(_binding.program_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.Expression foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Calculation)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Calculation _calculation = (com.biosimilarity.lift.lib.scalar.Absyn.Calculation) foo;
       render("(");
       render("Calculation");
       sh(_calculation.arithmeticexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Embedding)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Embedding _embedding = (com.biosimilarity.lift.lib.scalar.Absyn.Embedding) foo;
       render("(");
       render("Embedding");
       sh(_embedding.program_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.ArithmeticExpr foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Summation)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Summation _summation = (com.biosimilarity.lift.lib.scalar.Absyn.Summation) foo;
       render("(");
       render("Summation");
       sh(_summation.arithmeticexpr_1);
       sh(_summation.arithmeticexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Multiplication)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Multiplication _multiplication = (com.biosimilarity.lift.lib.scalar.Absyn.Multiplication) foo;
       render("(");
       render("Multiplication");
       sh(_multiplication.arithmeticexpr_1);
       sh(_multiplication.arithmeticexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Negation)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Negation _negation = (com.biosimilarity.lift.lib.scalar.Absyn.Negation) foo;
       render("(");
       render("Negation");
       sh(_negation.arithmeticexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Function)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Function _function = (com.biosimilarity.lift.lib.scalar.Absyn.Function) foo;
       render("(");
       render("Function");
       sh(_function.lambdaexpr_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.LambdaExpr foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Application)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Application _application = (com.biosimilarity.lift.lib.scalar.Absyn.Application) foo;
       render("(");
       render("Application");
       sh(_application.lambdaexpr_);
       render("[");
       sh(_application.listlambdaexpr_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Abstraction)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Abstraction _abstraction = (com.biosimilarity.lift.lib.scalar.Absyn.Abstraction) foo;
       render("(");
       render("Abstraction");
       render("[");
       sh(_abstraction.listvariableexpr_);
       render("]");
       sh(_abstraction.program_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Mention)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Mention _mention = (com.biosimilarity.lift.lib.scalar.Absyn.Mention) foo;
       render("(");
       render("Mention");
       sh(_mention.variableexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Value)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Value _value = (com.biosimilarity.lift.lib.scalar.Absyn.Value) foo;
       render("(");
       render("Value");
       sh(_value.valueexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Association)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Association _association = (com.biosimilarity.lift.lib.scalar.Absyn.Association) foo;
       render("(");
       render("Association");
       sh(_association.arithmeticexpr_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.VariableExpr foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Atom)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Atom _atom = (com.biosimilarity.lift.lib.scalar.Absyn.Atom) foo;
       render("(");
       render("Atom");
       sh(_atom.ident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Transcription)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Transcription _transcription = (com.biosimilarity.lift.lib.scalar.Absyn.Transcription) foo;
       render("(");
       render("Transcription");
       sh(_transcription.expression_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.ValueExpr foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Listed)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Listed _listed = (com.biosimilarity.lift.lib.scalar.Absyn.Listed) foo;
       render("(");
       render("Listed");
       render("[");
       sh(_listed.listexpression_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Quantity)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Quantity _quantity = (com.biosimilarity.lift.lib.scalar.Absyn.Quantity) foo;
       render("(");
       render("Quantity");
       sh(_quantity.numeric_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Quality)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Quality _quality = (com.biosimilarity.lift.lib.scalar.Absyn.Quality) foo;
       render("(");
       render("Quality");
       sh(_quality.logical_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Utterance)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Utterance _utterance = (com.biosimilarity.lift.lib.scalar.Absyn.Utterance) foo;
       render("(");
       render("Utterance");
       sh(_utterance.string_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.Numeric foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Measure)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Measure _measure = (com.biosimilarity.lift.lib.scalar.Absyn.Measure) foo;
       render("(");
       render("Measure");
       sh(_measure.double_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Count)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Count _count = (com.biosimilarity.lift.lib.scalar.Absyn.Count) foo;
       render("(");
       render("Count");
       sh(_count.integer_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.Logical foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Verity)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Verity _verity = (com.biosimilarity.lift.lib.scalar.Absyn.Verity) foo;
       render("Verity");
    }
    if (foo instanceof com.biosimilarity.lift.lib.scalar.Absyn.Absurdity)
    {
       com.biosimilarity.lift.lib.scalar.Absyn.Absurdity _absurdity = (com.biosimilarity.lift.lib.scalar.Absyn.Absurdity) foo;
       render("Absurdity");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.ListVariableExpr foo)
  {
     for (java.util.Iterator<VariableExpr> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.ListExpression foo)
  {
     for (java.util.Iterator<Expression> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.lib.scalar.Absyn.ListLambdaExpr foo)
  {
     for (java.util.Iterator<LambdaExpr> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }


  private static void pp(Integer n, int _i_) { buf_.append(n); buf_.append(" "); }
  private static void pp(Double d, int _i_) { buf_.append(d); buf_.append(" "); }
  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(" "); }
  private static void pp(Character c, int _i_) { buf_.append("'" + c.toString() + "'"); buf_.append(" "); }
  private static void sh(Integer n) { render(n.toString()); }
  private static void sh(Double d) { render(d.toString()); }
  private static void sh(Character c) { render(c.toString()); }
  private static void sh(String s) { printQuoted(s); }
  private static void printQuoted(String s) { render("\"" + s + "\""); }
  private static void indent()
  {
    int n = _n_;
    while (n > 0)
    {
      buf_.append(" ");
      n--;
    }
  }
  private static void backup()
  {
     if (buf_.charAt(buf_.length() - 1) == ' ') {
      buf_.setLength(buf_.length() - 1);
    }
  }
  private static void trim()
  {
     while (buf_.length() > 0 && buf_.charAt(0) == ' ')
        buf_.deleteCharAt(0); 
    while (buf_.length() > 0 && buf_.charAt(buf_.length()-1) == ' ')
        buf_.deleteCharAt(buf_.length()-1);
  }
  private static int _n_ = 0;
  private static StringBuilder buf_ = new StringBuilder(INITIAL_BUFFER_SIZE);
}

