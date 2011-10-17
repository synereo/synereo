package com.biosimilarity.seleKt.model.ill.lang.illtl;
import com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.*;

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
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ListFormalExpr foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ListFormalExpr foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application _application = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_application.rllexpr_, 0);
       pp(_application.listrllexpr_, 1);
       render(";");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation _separation = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_separation.rllexpr_1, 1);
       render("(x)");
       pp(_separation.rllexpr_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion _inclusion = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion) foo;
       if (_i_ > 1) render(_L_PAREN);
       render("<");
       pp(_inclusion.rllexpr_1, 2);
       render(",");
       pp(_inclusion.rllexpr_2, 2);
       render(">");
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction _abstraction = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("lambda");
       pp(_abstraction.listformalexpr_, 0);
       render(".");
       pp(_abstraction.rllexpr_, 2);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft _injectionleft = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("inl");
       render("(");
       pp(_injectionleft.rllexpr_, 2);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight _injectionright = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("inr");
       render("(");
       pp(_injectionright.rllexpr_, 2);
       render(")");
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration _duration = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("!");
       pp(_duration.rllexpr_, 2);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction _deconstruction = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("let");
       pp(_deconstruction.rllexpr_1, 2);
       render("be");
       pp(_deconstruction.rllptrn_, 0);
       render("in");
       pp(_deconstruction.rllexpr_2, 2);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection _selection = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection) foo;
       if (_i_ > 2) render(_L_PAREN);
       render("case");
       pp(_selection.rllexpr_1, 2);
       render("of");
       pp(_selection.rllleftptrn_, 0);
       render("=>");
       pp(_selection.rllexpr_2, 2);
       render(";");
       pp(_selection.rllrightptrn_, 0);
       render("=>");
       pp(_selection.rllexpr_3, 2);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention _mention = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_mention.formalexpr_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value _value = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value) foo;
       if (_i_ > 3) render(_L_PAREN);
       pp(_value.valueexpr_, 0);
       if (_i_ > 3) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft _inleft = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("inl");
       render("(");
       pp(_inleft.formalexpr_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight _inright = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("inr");
       render("(");
       pp(_inright.formalexpr_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn _separationptn = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_separationptn.formalexpr_1, 0);
       render("(x)");
       pp(_separationptn.formalexpr_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn _duplicationptn = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_duplicationptn.formalexpr_1, 0);
       render("@");
       pp(_duplicationptn.formalexpr_2, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft _inclusionleft = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("<");
       pp(_inclusionleft.formalexpr_, 0);
       render(",");
       render("_");
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight _inclusionright = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("<");
       render("_");
       render(",");
       pp(_inclusionright.formalexpr_, 0);
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction _extraction = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("!");
       pp(_extraction.formalexpr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard _wildcard = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("_");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn _unitptn = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("*");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription _transcription = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       render("<");
       pp(_transcription.rllexpr_, 2);
       render(">");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral _atomliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atomliteral.ident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral _decimalliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_decimalliteral.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral _integerliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_integerliteral.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral _stringliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_stringliteral.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral _unitliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("*");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ListRLLExpr foo, int _i_)
  {
     for (java.util.Iterator<RLLExpr> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ListFormalExpr foo, int _i_)
  {
     for (java.util.Iterator<FormalExpr> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLExpr foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application _application = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Application) foo;
       render("(");
       render("Application");
       sh(_application.rllexpr_);
       render("[");
       sh(_application.listrllexpr_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation _separation = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Separation) foo;
       render("(");
       render("Separation");
       sh(_separation.rllexpr_1);
       sh(_separation.rllexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion _inclusion = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Inclusion) foo;
       render("(");
       render("Inclusion");
       sh(_inclusion.rllexpr_1);
       sh(_inclusion.rllexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction _abstraction = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Abstraction) foo;
       render("(");
       render("Abstraction");
       render("[");
       sh(_abstraction.listformalexpr_);
       render("]");
       sh(_abstraction.rllexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft _injectionleft = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionLeft) foo;
       render("(");
       render("InjectionLeft");
       sh(_injectionleft.rllexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight _injectionright = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InjectionRight) foo;
       render("(");
       render("InjectionRight");
       sh(_injectionright.rllexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration _duration = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Duration) foo;
       render("(");
       render("Duration");
       sh(_duration.rllexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction _deconstruction = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Deconstruction) foo;
       render("(");
       render("Deconstruction");
       sh(_deconstruction.rllexpr_1);
       sh(_deconstruction.rllptrn_);
       sh(_deconstruction.rllexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection _selection = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Selection) foo;
       render("(");
       render("Selection");
       sh(_selection.rllexpr_1);
       sh(_selection.rllleftptrn_);
       sh(_selection.rllexpr_2);
       sh(_selection.rllrightptrn_);
       sh(_selection.rllexpr_3);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention _mention = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Mention) foo;
       render("(");
       render("Mention");
       sh(_mention.formalexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value _value = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Value) foo;
       render("(");
       render("Value");
       sh(_value.valueexpr_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLLeftPtrn foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft _inleft = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InLeft) foo;
       render("(");
       render("InLeft");
       sh(_inleft.formalexpr_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLRightPtrn foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight _inright = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InRight) foo;
       render("(");
       render("InRight");
       sh(_inright.formalexpr_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.RLLPtrn foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn _separationptn = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.SeparationPtn) foo;
       render("(");
       render("SeparationPtn");
       sh(_separationptn.formalexpr_1);
       sh(_separationptn.formalexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn _duplicationptn = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DuplicationPtn) foo;
       render("(");
       render("DuplicationPtn");
       sh(_duplicationptn.formalexpr_1);
       sh(_duplicationptn.formalexpr_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft _inclusionleft = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionLeft) foo;
       render("(");
       render("InclusionLeft");
       sh(_inclusionleft.formalexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight _inclusionright = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.InclusionRight) foo;
       render("(");
       render("InclusionRight");
       sh(_inclusionright.formalexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction _extraction = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Extraction) foo;
       render("(");
       render("Extraction");
       sh(_extraction.formalexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard _wildcard = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Wildcard) foo;
       render("Wildcard");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn _unitptn = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitPtn) foo;
       render("UnitPtn");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.FormalExpr foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription _transcription = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.Transcription) foo;
       render("(");
       render("Transcription");
       sh(_transcription.rllexpr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral _atomliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.AtomLiteral) foo;
       render("(");
       render("AtomLiteral");
       sh(_atomliteral.ident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ValueExpr foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral _decimalliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.DecimalLiteral) foo;
       render("(");
       render("DecimalLiteral");
       sh(_decimalliteral.double_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral _integerliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.IntegerLiteral) foo;
       render("(");
       render("IntegerLiteral");
       sh(_integerliteral.integer_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral _stringliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.StringLiteral) foo;
       render("(");
       render("StringLiteral");
       sh(_stringliteral.string_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral)
    {
       com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral _unitliteral = (com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.UnitLiteral) foo;
       render("UnitLiteral");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ListRLLExpr foo)
  {
     for (java.util.Iterator<RLLExpr> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.lang.illtl.Absyn.ListFormalExpr foo)
  {
     for (java.util.Iterator<FormalExpr> it = foo.iterator(); it.hasNext();)
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

