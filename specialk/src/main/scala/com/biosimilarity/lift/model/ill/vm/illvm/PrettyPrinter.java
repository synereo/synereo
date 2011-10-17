package com.biosimilarity.seleKt.model.ill.vm.illvm;
import com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.*;

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
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListFrame foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListFrame foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListInstruction foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListInstruction foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListEnvOrVal foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListEnvOrVal foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListValue foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListValue foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE _machine = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("ILLVM");
       render("(");
       pp(_machine.stack_, 0);
       render(";");
       pp(_machine.env_, 0);
       render(";");
       pp(_machine.illcode_, 0);
       render(";");
       pp(_machine.dump_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK _mstack = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_mstack.listenvorval_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP _mdump = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_mdump.listframe_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV _stackenv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_stackenv.env_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL _stackval = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_stackval.value_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME _stackframe = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_stackframe.stack_, 0);
       render(";");
       pp(_stackframe.env_, 0);
       render(";");
       pp(_stackframe.illcode_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ _codeseq = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_codeseq.listinstruction_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV _pushenv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pushenv.illpushenv_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD _head = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_head.illhd_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL _tail = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tail.illtl_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET _ret = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ret.illret_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH _push = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_push.illpush_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP _pop = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pop.illpop_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL _makefcl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("MAKEFCL");
       render("(");
       pp(_makefcl.illcode_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP _ap = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ap.illap_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT _unit = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_unit.illunit_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT _ununit = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_ununit.illununit_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR _pair = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_pair.illpair_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR _unpair = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_unpair.illunpair_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL _makeccl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("MAKECCL");
       render("(");
       pp(_makeccl.illcode_1, 0);
       render(";");
       pp(_makeccl.illcode_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST _fst = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_fst.illfst_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND _snd = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_snd.illsnd_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL _inl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_inl.illinl_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR _inr = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_inr.illinr_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE _case = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("CASE");
       render("(");
       pp(_case.illcode_1, 0);
       render(";");
       pp(_case.illcode_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL _makeocl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("MAKEOCL");
       render("(");
       pp(_makeocl.illcode_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ _read = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_read.illread_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP _dup = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_dup.illdup_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV _unitv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("*");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV _pairv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("(");
       pp(_pairv.value_1, 0);
       render(",");
       pp(_pairv.value_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV _inlv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("inl");
       render("(");
       pp(_inlv.value_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV _inrv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("inr");
       render("(");
       pp(_inrv.value_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV _fclv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("fcl");
       render("(");
       pp(_fclv.illcode_, 0);
       render(";");
       pp(_fclv.env_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV _cclv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("ccl");
       render("(");
       pp(_cclv.illcode_1, 0);
       render(";");
       pp(_cclv.illcode_2, 0);
       render(";");
       pp(_cclv.env_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV _oclv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("ocl");
       render("(");
       pp(_oclv.illcode_, 0);
       render(";");
       pp(_oclv.env_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT _environment = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_environment.listvalue_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListFrame foo, int _i_)
  {
     for (java.util.Iterator<Frame> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListInstruction foo, int _i_)
  {
     for (java.util.Iterator<Instruction> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListEnvOrVal foo, int _i_)
  {
     for (java.util.Iterator<EnvOrVal> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListValue foo, int _i_)
  {
     for (java.util.Iterator<Value> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.State foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE _machine = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MACHINE) foo;
       render("(");
       render("MACHINE");
       sh(_machine.stack_);
       sh(_machine.env_);
       sh(_machine.illcode_);
       sh(_machine.dump_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Stack foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK _mstack = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MSTACK) foo;
       render("(");
       render("MSTACK");
       render("[");
       sh(_mstack.listenvorval_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Dump foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP _mdump = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MDUMP) foo;
       render("(");
       render("MDUMP");
       render("[");
       sh(_mdump.listframe_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.EnvOrVal foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV _stackenv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKENV) foo;
       render("(");
       render("STACKENV");
       sh(_stackenv.env_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL _stackval = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKVAL) foo;
       render("(");
       render("STACKVAL");
       sh(_stackval.value_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Frame foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME _stackframe = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.STACKFRAME) foo;
       render("(");
       render("STACKFRAME");
       sh(_stackframe.stack_);
       sh(_stackframe.env_);
       sh(_stackframe.illcode_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ILLCode foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ _codeseq = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CODESEQ) foo;
       render("(");
       render("CODESEQ");
       render("[");
       sh(_codeseq.listinstruction_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Instruction foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV _pushenv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSHENV) foo;
       render("(");
       render("PUSHENV");
       sh(_pushenv.illpushenv_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD _head = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.HEAD) foo;
       render("(");
       render("HEAD");
       sh(_head.illhd_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL _tail = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.TAIL) foo;
       render("(");
       render("TAIL");
       sh(_tail.illtl_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET _ret = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.RET) foo;
       render("(");
       render("RET");
       sh(_ret.illret_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH _push = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PUSH) foo;
       render("(");
       render("PUSH");
       sh(_push.illpush_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP _pop = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.POP) foo;
       render("(");
       render("POP");
       sh(_pop.illpop_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL _makefcl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEFCL) foo;
       render("(");
       render("MAKEFCL");
       sh(_makefcl.illcode_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP _ap = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.AP) foo;
       render("(");
       render("AP");
       sh(_ap.illap_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT _unit = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNIT) foo;
       render("(");
       render("UNIT");
       sh(_unit.illunit_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT _ununit = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNUNIT) foo;
       render("(");
       render("UNUNIT");
       sh(_ununit.illununit_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR _pair = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PAIR) foo;
       render("(");
       render("PAIR");
       sh(_pair.illpair_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR _unpair = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UNPAIR) foo;
       render("(");
       render("UNPAIR");
       sh(_unpair.illunpair_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL _makeccl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKECCL) foo;
       render("(");
       render("MAKECCL");
       sh(_makeccl.illcode_1);
       sh(_makeccl.illcode_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST _fst = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FST) foo;
       render("(");
       render("FST");
       sh(_fst.illfst_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND _snd = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.SND) foo;
       render("(");
       render("SND");
       sh(_snd.illsnd_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL _inl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INL) foo;
       render("(");
       render("INL");
       sh(_inl.illinl_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR _inr = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.INR) foo;
       render("(");
       render("INR");
       sh(_inr.illinr_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE _case = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CASE) foo;
       render("(");
       render("CASE");
       sh(_case.illcode_1);
       sh(_case.illcode_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL _makeocl = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.MAKEOCL) foo;
       render("(");
       render("MAKEOCL");
       sh(_makeocl.illcode_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ _read = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.READ) foo;
       render("(");
       render("READ");
       sh(_read.illread_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP _dup = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.DUP) foo;
       render("(");
       render("DUP");
       sh(_dup.illdup_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Value foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV _unitv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.UnitV) foo;
       render("UnitV");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV _pairv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.PairV) foo;
       render("(");
       render("PairV");
       sh(_pairv.value_1);
       sh(_pairv.value_2);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV _inlv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InlV) foo;
       render("(");
       render("InlV");
       sh(_inlv.value_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV _inrv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.InrV) foo;
       render("(");
       render("InrV");
       sh(_inrv.value_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV _fclv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.FclV) foo;
       render("(");
       render("FclV");
       sh(_fclv.illcode_);
       sh(_fclv.env_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV _cclv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.CclV) foo;
       render("(");
       render("CclV");
       sh(_cclv.illcode_1);
       sh(_cclv.illcode_2);
       sh(_cclv.env_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV _oclv = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.OclV) foo;
       render("(");
       render("OclV");
       sh(_oclv.illcode_);
       sh(_oclv.env_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.Env foo)
  {
    if (foo instanceof com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT)
    {
       com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT _environment = (com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ENVIRONMENT) foo;
       render("(");
       render("ENVIRONMENT");
       render("[");
       sh(_environment.listvalue_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListFrame foo)
  {
     for (java.util.Iterator<Frame> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListInstruction foo)
  {
     for (java.util.Iterator<Instruction> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListEnvOrVal foo)
  {
     for (java.util.Iterator<EnvOrVal> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.seleKt.model.ill.vm.illvm.Absyn.ListValue foo)
  {
     for (java.util.Iterator<Value> it = foo.iterator(); it.hasNext();)
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

