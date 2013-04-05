package com.biosimilarity.lift.lib.term.Prolog;
import com.biosimilarity.lift.lib.term.Prolog.Absyn.*;

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
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Term foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Term foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Var foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Var foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListPredicate foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListPredicate foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListTerm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListTerm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.APred)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.APred _apred = (com.biosimilarity.lift.lib.term.Prolog.Absyn.APred) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_apred.atom_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred _cpred = (com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_cpred.functor_, 0);
       render("(");
       pp(_cpred.listterm_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Term foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom _tatom = (com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tatom.atom_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT _vart = (com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_vart.var_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex _complex = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_complex.functor_, 0);
       render("(");
       pp(_complex.listterm_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.TList)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.TList _tlist = (com.biosimilarity.lift.lib.term.Prolog.Absyn.TList) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tlist.lyst_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm _atm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atm.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm _eatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("'");
       pp(_eatm.ident_, 0);
       render("'");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm _batm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_batm.boole_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm _stratm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_stratm.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm _intatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_intatm.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm _fltatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_fltatm.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm _fatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_fatm.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity _verity = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("true");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity _absurdity = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("false");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Var foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.V)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.V _v = (com.biosimilarity.lift.lib.term.Prolog.Absyn.V) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_v.uident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.A)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.A _a = (com.biosimilarity.lift.lib.term.Prolog.Absyn.A) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_a.wild_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty _empty = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum _enum = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_enum.listterm_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons _cons = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_cons.listterm_, 0);
       render("|");
       pp(_cons.lyst_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV _consv = (com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_consv.listterm_, 0);
       render("|");
       pp(_consv.var_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListPredicate foo, int _i_)
  {
     for (java.util.Iterator<Predicate> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListTerm foo, int _i_)
  {
     for (java.util.Iterator<Term> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Predicate foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.APred)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.APred _apred = (com.biosimilarity.lift.lib.term.Prolog.Absyn.APred) foo;
       render("(");
       render("APred");
       sh(_apred.atom_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred _cpred = (com.biosimilarity.lift.lib.term.Prolog.Absyn.CPred) foo;
       render("(");
       render("CPred");
       sh(_cpred.functor_);
       render("[");
       sh(_cpred.listterm_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Term foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom _tatom = (com.biosimilarity.lift.lib.term.Prolog.Absyn.TAtom) foo;
       render("(");
       render("TAtom");
       sh(_tatom.atom_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT _vart = (com.biosimilarity.lift.lib.term.Prolog.Absyn.VarT) foo;
       render("(");
       render("VarT");
       sh(_vart.var_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex _complex = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Complex) foo;
       render("(");
       render("Complex");
       sh(_complex.functor_);
       render("[");
       sh(_complex.listterm_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.TList)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.TList _tlist = (com.biosimilarity.lift.lib.term.Prolog.Absyn.TList) foo;
       render("(");
       render("TList");
       sh(_tlist.lyst_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Atom foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm _atm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Atm) foo;
       render("(");
       render("Atm");
       sh(_atm.lident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm _eatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.EAtm) foo;
       render("(");
       render("EAtm");
       sh(_eatm.ident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm _batm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.BAtm) foo;
       render("(");
       render("BAtm");
       sh(_batm.boole_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm _stratm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.StrAtm) foo;
       render("(");
       render("StrAtm");
       sh(_stratm.string_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm _intatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.IntAtm) foo;
       render("(");
       render("IntAtm");
       sh(_intatm.integer_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm _fltatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.FltAtm) foo;
       render("(");
       render("FltAtm");
       sh(_fltatm.double_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Functor foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm _fatm = (com.biosimilarity.lift.lib.term.Prolog.Absyn.FAtm) foo;
       render("(");
       render("FAtm");
       sh(_fatm.lident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Boole foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity _verity = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Verity) foo;
       render("Verity");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity _absurdity = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Absurdity) foo;
       render("Absurdity");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Var foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.V)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.V _v = (com.biosimilarity.lift.lib.term.Prolog.Absyn.V) foo;
       render("(");
       render("V");
       sh(_v.uident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.A)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.A _a = (com.biosimilarity.lift.lib.term.Prolog.Absyn.A) foo;
       render("(");
       render("A");
       sh(_a.wild_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.Lyst foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty _empty = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Empty) foo;
       render("Empty");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum _enum = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Enum) foo;
       render("(");
       render("Enum");
       render("[");
       sh(_enum.listterm_);
       render("]");
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons _cons = (com.biosimilarity.lift.lib.term.Prolog.Absyn.Cons) foo;
       render("(");
       render("Cons");
       render("[");
       sh(_cons.listterm_);
       render("]");
       sh(_cons.lyst_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV)
    {
       com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV _consv = (com.biosimilarity.lift.lib.term.Prolog.Absyn.ConsV) foo;
       render("(");
       render("ConsV");
       render("[");
       sh(_consv.listterm_);
       render("]");
       sh(_consv.var_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListPredicate foo)
  {
     for (java.util.Iterator<Predicate> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.lib.term.Prolog.Absyn.ListTerm foo)
  {
     for (java.util.Iterator<Term> it = foo.iterator(); it.hasNext();)
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

