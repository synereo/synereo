package com.biosimilarity.lift.model.specialK;
import com.biosimilarity.lift.model.specialK.Absyn.*;

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
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Agent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Agent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Abstraction foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Abstraction foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Concretion foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Concretion foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Pattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Pattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Symbol foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Symbol foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Variation foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Variation foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Information foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Information foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.Duality foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.Duality foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.ListAgent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.ListAgent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.ListGuardedAgent foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.ListGuardedAgent foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.model.specialK.Absyn.ListPattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.model.specialK.Absyn.ListPattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Agent foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Composition)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Composition _composition = (com.biosimilarity.lift.model.specialK.Absyn.Composition) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_composition.listagent_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Superposition)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Superposition _superposition = (com.biosimilarity.lift.model.specialK.Absyn.Superposition) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("switch");
       render("{");
       pp(_superposition.listguardedagent_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Replication)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Replication _replication = (com.biosimilarity.lift.model.specialK.Absyn.Replication) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("*");
       pp(_replication.variation_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Ingestion)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Ingestion _ingestion = (com.biosimilarity.lift.model.specialK.Absyn.Ingestion) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ingestion.pattern_, 0);
       render("?");
       pp(_ingestion.abstraction_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Excretion)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Excretion _excretion = (com.biosimilarity.lift.model.specialK.Absyn.Excretion) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_excretion.pattern_, 0);
       render("!");
       pp(_excretion.concretion_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Injection)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Injection _injection = (com.biosimilarity.lift.model.specialK.Absyn.Injection) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("case");
       pp(_injection.pattern_, 0);
       render("=>");
       pp(_injection.agent_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Abstraction foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Applicant)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Applicant _applicant = (com.biosimilarity.lift.model.specialK.Absyn.Applicant) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_applicant.variation_, 0);
       render(")");
       pp(_applicant.agent_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Concretion foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Applicand)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Applicand _applicand = (com.biosimilarity.lift.model.specialK.Absyn.Applicand) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_applicand.information_, 0);
       render(")");
       pp(_applicand.agent_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Pattern foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Element)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Element _element = (com.biosimilarity.lift.model.specialK.Absyn.Element) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_element.symbol_, 0);
       render("(");
       pp(_element.listpattern_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Variable)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Variable _variable = (com.biosimilarity.lift.model.specialK.Absyn.Variable) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_variable.variation_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Literal)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Literal _literal = (com.biosimilarity.lift.model.specialK.Absyn.Literal) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_literal.value_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Symbol foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Tag)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Tag _tag = (com.biosimilarity.lift.model.specialK.Absyn.Tag) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tag.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Variation foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Atomic)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Atomic _atomic = (com.biosimilarity.lift.model.specialK.Absyn.Atomic) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atomic.uident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Transcription)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Transcription _transcription = (com.biosimilarity.lift.model.specialK.Absyn.Transcription) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("@");
       pp(_transcription.agent_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Information foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Indirection)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Indirection _indirection = (com.biosimilarity.lift.model.specialK.Absyn.Indirection) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_indirection.variation_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Reflection)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Reflection _reflection = (com.biosimilarity.lift.model.specialK.Absyn.Reflection) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_reflection.agent_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Value foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral _booleanliteral = (com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_booleanliteral.duality_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.StringLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.StringLiteral _stringliteral = (com.biosimilarity.lift.model.specialK.Absyn.StringLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_stringliteral.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral _integerliteral = (com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_integerliteral.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral _doubleliteral = (com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_doubleliteral.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.Duality foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Verity)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Verity _verity = (com.biosimilarity.lift.model.specialK.Absyn.Verity) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("true");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Absurdity)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Absurdity _absurdity = (com.biosimilarity.lift.model.specialK.Absyn.Absurdity) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("false");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.ListAgent foo, int _i_)
  {
     for (java.util.Iterator<Agent> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(";");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.ListGuardedAgent foo, int _i_)
  {
     for (java.util.Iterator<GuardedAgent> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(";");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.model.specialK.Absyn.ListPattern foo, int _i_)
  {
     for (java.util.Iterator<Pattern> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Agent foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Composition)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Composition _composition = (com.biosimilarity.lift.model.specialK.Absyn.Composition) foo;
       render("(");
       render("Composition");
       render("[");
       sh(_composition.listagent_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Superposition)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Superposition _superposition = (com.biosimilarity.lift.model.specialK.Absyn.Superposition) foo;
       render("(");
       render("Superposition");
       render("[");
       sh(_superposition.listguardedagent_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Replication)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Replication _replication = (com.biosimilarity.lift.model.specialK.Absyn.Replication) foo;
       render("(");
       render("Replication");
       sh(_replication.variation_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Ingestion)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Ingestion _ingestion = (com.biosimilarity.lift.model.specialK.Absyn.Ingestion) foo;
       render("(");
       render("Ingestion");
       sh(_ingestion.pattern_);
       sh(_ingestion.abstraction_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Excretion)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Excretion _excretion = (com.biosimilarity.lift.model.specialK.Absyn.Excretion) foo;
       render("(");
       render("Excretion");
       sh(_excretion.pattern_);
       sh(_excretion.concretion_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.GuardedAgent foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Injection)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Injection _injection = (com.biosimilarity.lift.model.specialK.Absyn.Injection) foo;
       render("(");
       render("Injection");
       sh(_injection.pattern_);
       sh(_injection.agent_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Abstraction foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Applicant)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Applicant _applicant = (com.biosimilarity.lift.model.specialK.Absyn.Applicant) foo;
       render("(");
       render("Applicant");
       sh(_applicant.variation_);
       sh(_applicant.agent_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Concretion foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Applicand)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Applicand _applicand = (com.biosimilarity.lift.model.specialK.Absyn.Applicand) foo;
       render("(");
       render("Applicand");
       sh(_applicand.information_);
       sh(_applicand.agent_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Pattern foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Element)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Element _element = (com.biosimilarity.lift.model.specialK.Absyn.Element) foo;
       render("(");
       render("Element");
       sh(_element.symbol_);
       render("[");
       sh(_element.listpattern_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Variable)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Variable _variable = (com.biosimilarity.lift.model.specialK.Absyn.Variable) foo;
       render("(");
       render("Variable");
       sh(_variable.variation_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Literal)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Literal _literal = (com.biosimilarity.lift.model.specialK.Absyn.Literal) foo;
       render("(");
       render("Literal");
       sh(_literal.value_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Symbol foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Tag)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Tag _tag = (com.biosimilarity.lift.model.specialK.Absyn.Tag) foo;
       render("(");
       render("Tag");
       sh(_tag.lident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Variation foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Atomic)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Atomic _atomic = (com.biosimilarity.lift.model.specialK.Absyn.Atomic) foo;
       render("(");
       render("Atomic");
       sh(_atomic.uident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Transcription)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Transcription _transcription = (com.biosimilarity.lift.model.specialK.Absyn.Transcription) foo;
       render("(");
       render("Transcription");
       sh(_transcription.agent_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Information foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Indirection)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Indirection _indirection = (com.biosimilarity.lift.model.specialK.Absyn.Indirection) foo;
       render("(");
       render("Indirection");
       sh(_indirection.variation_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Reflection)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Reflection _reflection = (com.biosimilarity.lift.model.specialK.Absyn.Reflection) foo;
       render("(");
       render("Reflection");
       sh(_reflection.agent_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Value foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral _booleanliteral = (com.biosimilarity.lift.model.specialK.Absyn.BooleanLiteral) foo;
       render("(");
       render("BooleanLiteral");
       sh(_booleanliteral.duality_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.StringLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.StringLiteral _stringliteral = (com.biosimilarity.lift.model.specialK.Absyn.StringLiteral) foo;
       render("(");
       render("StringLiteral");
       sh(_stringliteral.string_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral _integerliteral = (com.biosimilarity.lift.model.specialK.Absyn.IntegerLiteral) foo;
       render("(");
       render("IntegerLiteral");
       sh(_integerliteral.integer_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral)
    {
       com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral _doubleliteral = (com.biosimilarity.lift.model.specialK.Absyn.DoubleLiteral) foo;
       render("(");
       render("DoubleLiteral");
       sh(_doubleliteral.double_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.Duality foo)
  {
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Verity)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Verity _verity = (com.biosimilarity.lift.model.specialK.Absyn.Verity) foo;
       render("Verity");
    }
    if (foo instanceof com.biosimilarity.lift.model.specialK.Absyn.Absurdity)
    {
       com.biosimilarity.lift.model.specialK.Absyn.Absurdity _absurdity = (com.biosimilarity.lift.model.specialK.Absyn.Absurdity) foo;
       render("Absurdity");
    }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.ListAgent foo)
  {
     for (java.util.Iterator<Agent> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.ListGuardedAgent foo)
  {
     for (java.util.Iterator<GuardedAgent> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.model.specialK.Absyn.ListPattern foo)
  {
     for (java.util.Iterator<Pattern> it = foo.iterator(); it.hasNext();)
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

