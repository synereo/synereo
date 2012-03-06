package com.biosimilarity.lift.lib.kvdbJSON;
import com.biosimilarity.lift.lib.kvdbJSON.Absyn.*;

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
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListQryElem foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListQryElem foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListSubstPair foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListSubstPair foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListURIPathElement foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListURIPathElement foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListDNSElement foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListDNSElement foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB _kvdbjustreqhb = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbjustreqhb.lblreqheader_, 0);
       render(",");
       pp(_kvdbjustreqhb.lblreqbody_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH _kvdbjustreqbh = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbjustreqbh.lblreqbody_, 0);
       render(",");
       pp(_kvdbjustreqbh.lblreqheader_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB _kvdbjustrsphb = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbjustrsphb.lblrspheader_, 0);
       render(",");
       pp(_kvdbjustrsphb.lblrspbody_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH _kvdbjustrspbh = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbjustrspbh.lblrspbody_, 0);
       render(",");
       pp(_kvdbjustrspbh.lblrspheader_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr _kvdblblreqhdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"headers\"");
       render(":");
       pp(_kvdblblreqhdr.reqheader_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr _kvdblblrsphdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"headers\"");
       render(":");
       pp(_kvdblblrsphdr.rspheader_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody _kvdblblreqbody = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"body\"");
       render(":");
       pp(_kvdblblreqbody.kvdbrequest_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody _kvdblblrspbody = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"body\"");
       render(":");
       pp(_kvdblblrspbody.kvdbresponse_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr _kvdbreqhdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("'");
       pp(_kvdbreqhdr.uri_1, 0);
       render("'");
       render(",");
       render("'");
       pp(_kvdbreqhdr.uri_2, 0);
       render("'");
       render(",");
       render("'");
       pp(_kvdbreqhdr.uuid_1, 0);
       render("'");
       render(",");
       render("'");
       pp(_kvdbreqhdr.uuid_2, 0);
       render("'");
       render(",");
       pp(_kvdbreqhdr.reqjust_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr _kvdbreqnohdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("{");
       render("\"testReqEmptyHdrs\"");
       render(":");
       render("null");
       render("}");
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr _kvdbrsphdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("'");
       pp(_kvdbrsphdr.uri_1, 0);
       render("'");
       render(",");
       render("'");
       pp(_kvdbrsphdr.uri_2, 0);
       render("'");
       render(",");
       render("'");
       pp(_kvdbrsphdr.uuid_1, 0);
       render("'");
       render(",");
       render("'");
       pp(_kvdbrsphdr.uuid_2, 0);
       render("'");
       render(",");
       pp(_kvdbrsphdr.rspjust_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr _kvdbrspnohdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("{");
       render("\"testRspEmptyHdrs\"");
       render(":");
       render("null");
       render("}");
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq _kvdbaskreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbaskreq.askreq_, 0);
       render(":");
       pp(_kvdbaskreq.askreqpacket_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq _kvdbtellreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbtellreq.tellreq_, 0);
       render(":");
       pp(_kvdbtellreq.tellreqpacket_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq _kvdbnoreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("{");
       render("\"testReqEmptyBody\"");
       render(":");
       render("null");
       render("}");
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp _kvdbaskrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbaskrsp.askrsp_, 0);
       render(":");
       pp(_kvdbaskrsp.askrsppacket_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp _kvdbtellrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbtellrsp.tellrsp_, 0);
       render(":");
       pp(_kvdbtellrsp.tellrsppacket_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp _kvdbnorsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       render("{");
       render("\"testRspEmptyBody\"");
       render(":");
       render("null");
       render("}");
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData _kvdbaskreqdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"ask\"");
       render(":");
       pp(_kvdbaskreqdata.pattern_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData _kvdbaskrspdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"answer\"");
       render(":");
       render("[");
       pp(_kvdbaskrspdata.pattern_, 0);
       render(",");
       pp(_kvdbaskrspdata.substitution_, 0);
       render(",");
       pp(_kvdbaskrspdata.blob_, 0);
       render("]");
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData _kvdbtellreqdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"tell\"");
       render(":");
       render("[");
       pp(_kvdbtellreqdata.pattern_, 0);
       render(",");
       pp(_kvdbtellreqdata.blob_, 0);
       render("]");
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData _kvdbtellrspdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"acknowledge\"");
       render(":");
       pp(_kvdbtellrspdata.status_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone _kvdbreqjustnone = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"response\"");
       render(":");
       render("null");
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome _kvdbreqjustsome = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"response\"");
       render(":");
       render("[");
       pp(_kvdbreqjustsome.uuid_, 0);
       render("]");
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone _kvdbrspjustnone = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"request\"");
       render(":");
       render("null");
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome _kvdbrspjustsome = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       render("\"request\"");
       render(":");
       render("[");
       pp(_kvdbrspjustsome.uuid_, 0);
       render("]");
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq _kvdbgetreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"getRequest\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq _kvdbfetchreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"fetchRequest\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq _kvdbsubscribereq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"subscribeRequest\"");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq _kvdbputreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"putRequest\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq _kvdbstorereq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"storeRequest\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq _kvdbpublishreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"publishRequest\"");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp _kvdbgetrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"getResponse\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp _kvdbfetchrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"fetchResponse\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp _kvdbsubscribersp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"subscribeResponse\"");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp _kvdbputrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"putResponse\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp _kvdbstorersp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"storeResponse\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp _kvdbpublishrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"publishResponse\"");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk _kvdbstatusok = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"ok\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk _kvdbstatusnotok = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("\"notok\"");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode _kvdbstatuscode = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_kvdbstatuscode.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr _kvdbstatusstr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_kvdbstatusstr.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed _qpointed = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qpointed.qryterm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob _qblob = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_qblob.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst _kvdbsubst = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       pp(_kvdbsubst.listsubstpair_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair _kvdbsubstpair = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_kvdbsubstpair.varuident_, 0);
       render(":");
       pp(_kvdbsubstpair.qryterm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm _qterm = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("{");
       printQuoted(_qterm.string_);
       render(":");
       pp(_qterm.qryarray_, 0);
       render("}");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar _qvar = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qvar.varuident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal _qval = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qval.qryvalue_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic _qatomic = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qatomic.qrygrndlit_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl _qcoll = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qcoll.qryarray_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp _qcomp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qcomp.qryterm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray _qarray = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("[");
       pp(_qarray.listqryelem_, 0);
       render("]");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr _qstr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr) foo;
       if (_i_ > 0) render(_L_PAREN);
       printQuoted(_qstr.string_);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum _qnum = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qnum.qrynum_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool _qbool = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qbool.qrybool_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul _qnul = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("null");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru _qtru = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("true");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal _qfal = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("false");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt _qint = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl _qdbl = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_qdbl.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI _tokenuri = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_tokenuri.primuri_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI _basicuri = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_basicuri.urischeme_, 0);
       render(":");
       pp(_basicuri.uripath_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI _nulluri = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("null");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath _locatedtedpath = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("/");
       pp(_locatedtedpath.urilocation_, 0);
       pp(_locatedtedpath.urirelativepath_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath _relativepath = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("/");
       pp(_relativepath.urirelativepath_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation _urinetlocation = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_urinetlocation.uriroot_, 0);
       pp(_urinetlocation.urirsrclocation_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc _urirsrcportloc = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_urirsrcportloc.netlocation_, 0);
       render(":");
       pp(_urirsrcportloc.port_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc _urirsrcloc = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_urirsrcloc.netlocation_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath _slashpath = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_slashpath.uriroot_, 0);
       pp(_slashpath.listuripathelement_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin _uriorigin = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("/");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr _dnsaddr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_dnsaddr.listdnselement_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme _atomscheme = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atomscheme.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement _atompathelement = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atompathelement.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement _atomdnselement = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atomdnselement.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort _atomport = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_atomport.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID foo, int _i_)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID _kvdbluuid = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_kvdbluuid.lident_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID _kvdbprimuuid = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_kvdbprimuuid.primuuid_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID _kvdbnulluuid = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("null");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListQryElem foo, int _i_)
  {
     for (java.util.Iterator<QryElem> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListSubstPair foo, int _i_)
  {
     for (java.util.Iterator<SubstPair> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(",");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListURIPathElement foo, int _i_)
  {
     for (java.util.Iterator<URIPathElement> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render("/");
       } else {
         render("");
       }
     }
  }

  private static void pp(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListDNSElement foo, int _i_)
  {
     for (java.util.Iterator<DNSElement> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), 0);
       if (it.hasNext()) {
         render(".");
       } else {
         render("");
       }
     }
  }


  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB _kvdbjustreqhb = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqHB) foo;
       render("(");
       render("KVDBJustReqHB");
       sh(_kvdbjustreqhb.lblreqheader_);
       sh(_kvdbjustreqhb.lblreqbody_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH _kvdbjustreqbh = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustReqBH) foo;
       render("(");
       render("KVDBJustReqBH");
       sh(_kvdbjustreqbh.lblreqbody_);
       sh(_kvdbjustreqbh.lblreqheader_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB _kvdbjustrsphb = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspHB) foo;
       render("(");
       render("KVDBJustRspHB");
       sh(_kvdbjustrsphb.lblrspheader_);
       sh(_kvdbjustrsphb.lblrspbody_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH _kvdbjustrspbh = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBJustRspBH) foo;
       render("(");
       render("KVDBJustRspBH");
       sh(_kvdbjustrspbh.lblrspbody_);
       sh(_kvdbjustrspbh.lblrspheader_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqHeader foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr _kvdblblreqhdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqHdr) foo;
       render("(");
       render("KVDBLblReqHdr");
       sh(_kvdblblreqhdr.reqheader_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspHeader foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr _kvdblblrsphdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspHdr) foo;
       render("(");
       render("KVDBLblRspHdr");
       sh(_kvdblblrsphdr.rspheader_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblReqBody foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody _kvdblblreqbody = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblReqBody) foo;
       render("(");
       render("KVDBLblReqBody");
       sh(_kvdblblreqbody.kvdbrequest_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.LblRspBody foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody _kvdblblrspbody = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLblRspBody) foo;
       render("(");
       render("KVDBLblRspBody");
       sh(_kvdblblrspbody.kvdbresponse_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqHeader foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr _kvdbreqhdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqHdr) foo;
       render("(");
       render("KVDBReqHdr");
       sh(_kvdbreqhdr.uri_1);
       sh(_kvdbreqhdr.uri_2);
       sh(_kvdbreqhdr.uuid_1);
       sh(_kvdbreqhdr.uuid_2);
       sh(_kvdbreqhdr.reqjust_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr _kvdbreqnohdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqNoHdr) foo;
       render("KVDBReqNoHdr");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspHeader foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr _kvdbrsphdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspHdr) foo;
       render("(");
       render("KVDBRspHdr");
       sh(_kvdbrsphdr.uri_1);
       sh(_kvdbrsphdr.uri_2);
       sh(_kvdbrsphdr.uuid_1);
       sh(_kvdbrsphdr.uuid_2);
       sh(_kvdbrsphdr.rspjust_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr _kvdbrspnohdr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspNoHdr) foo;
       render("KVDBRspNoHdr");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRequest foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq _kvdbaskreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReq) foo;
       render("(");
       render("KVDBAskReq");
       sh(_kvdbaskreq.askreq_);
       sh(_kvdbaskreq.askreqpacket_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq _kvdbtellreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReq) foo;
       render("(");
       render("KVDBTellReq");
       sh(_kvdbtellreq.tellreq_);
       sh(_kvdbtellreq.tellreqpacket_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq _kvdbnoreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoReq) foo;
       render("KVDBNoReq");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBResponse foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp _kvdbaskrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRsp) foo;
       render("(");
       render("KVDBAskRsp");
       sh(_kvdbaskrsp.askrsp_);
       sh(_kvdbaskrsp.askrsppacket_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp _kvdbtellrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRsp) foo;
       render("(");
       render("KVDBTellRsp");
       sh(_kvdbtellrsp.tellrsp_);
       sh(_kvdbtellrsp.tellrsppacket_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp _kvdbnorsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNoRsp) foo;
       render("KVDBNoRsp");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReqPacket foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData _kvdbaskreqdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskReqData) foo;
       render("(");
       render("KVDBAskReqData");
       sh(_kvdbaskreqdata.pattern_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRspPacket foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData _kvdbaskrspdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBAskRspData) foo;
       render("(");
       render("KVDBAskRspData");
       sh(_kvdbaskrspdata.pattern_);
       sh(_kvdbaskrspdata.substitution_);
       sh(_kvdbaskrspdata.blob_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReqPacket foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData _kvdbtellreqdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellReqData) foo;
       render("(");
       render("KVDBTellReqData");
       sh(_kvdbtellreqdata.pattern_);
       sh(_kvdbtellreqdata.blob_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRspPacket foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData _kvdbtellrspdata = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBTellRspData) foo;
       render("(");
       render("KVDBTellRspData");
       sh(_kvdbtellrspdata.status_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ReqJust foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone _kvdbreqjustnone = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustNone) foo;
       render("KVDBReqJustNone");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome _kvdbreqjustsome = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBReqJustSome) foo;
       render("(");
       render("KVDBReqJustSome");
       sh(_kvdbreqjustsome.uuid_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.RspJust foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone _kvdbrspjustnone = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustNone) foo;
       render("KVDBRspJustNone");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome _kvdbrspjustsome = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBRspJustSome) foo;
       render("(");
       render("KVDBRspJustSome");
       sh(_kvdbrspjustsome.uuid_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskReq foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq _kvdbgetreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetReq) foo;
       render("KVDBGetReq");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq _kvdbfetchreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchReq) foo;
       render("KVDBFetchReq");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq _kvdbsubscribereq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeReq) foo;
       render("KVDBSubscribeReq");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellReq foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq _kvdbputreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutReq) foo;
       render("KVDBPutReq");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq _kvdbstorereq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreReq) foo;
       render("KVDBStoreReq");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq _kvdbpublishreq = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishReq) foo;
       render("KVDBPublishReq");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.AskRsp foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp _kvdbgetrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBGetRsp) foo;
       render("KVDBGetRsp");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp _kvdbfetchrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBFetchRsp) foo;
       render("KVDBFetchRsp");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp _kvdbsubscribersp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubscribeRsp) foo;
       render("KVDBSubscribeRsp");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.TellRsp foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp _kvdbputrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPutRsp) foo;
       render("KVDBPutRsp");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp _kvdbstorersp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStoreRsp) foo;
       render("KVDBStoreRsp");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp _kvdbpublishrsp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPublishRsp) foo;
       render("KVDBPublishRsp");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Status foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk _kvdbstatusok = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusOk) foo;
       render("KVDBStatusOk");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk _kvdbstatusnotok = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusNotOk) foo;
       render("KVDBStatusNotOk");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode _kvdbstatuscode = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusCode) foo;
       render("(");
       render("KVDBStatusCode");
       sh(_kvdbstatuscode.integer_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr _kvdbstatusstr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBStatusStr) foo;
       render("(");
       render("KVDBStatusStr");
       sh(_kvdbstatusstr.string_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Pattern foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed _qpointed = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QPointed) foo;
       render("(");
       render("QPointed");
       sh(_qpointed.qryterm_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Blob foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob _qblob = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBlob) foo;
       render("(");
       render("QBlob");
       sh(_qblob.string_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Substitution foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst _kvdbsubst = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubst) foo;
       render("(");
       render("KVDBSubst");
       render("[");
       sh(_kvdbsubst.listsubstpair_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.SubstPair foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair _kvdbsubstpair = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBSubstPair) foo;
       render("(");
       render("KVDBSubstPair");
       sh(_kvdbsubstpair.varuident_);
       sh(_kvdbsubstpair.qryterm_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryTerm foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm _qterm = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTerm) foo;
       render("(");
       render("QTerm");
       sh(_qterm.string_);
       sh(_qterm.qryarray_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryElem foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar _qvar = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVar) foo;
       render("(");
       render("QVar");
       sh(_qvar.varuident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal _qval = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QVal) foo;
       render("(");
       render("QVal");
       sh(_qval.qryvalue_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryValue foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic _qatomic = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QAtomic) foo;
       render("(");
       render("QAtomic");
       sh(_qatomic.qrygrndlit_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl _qcoll = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QColl) foo;
       render("(");
       render("QColl");
       sh(_qcoll.qryarray_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp _qcomp = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QComp) foo;
       render("(");
       render("QComp");
       sh(_qcomp.qryterm_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryArray foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray _qarray = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QArray) foo;
       render("(");
       render("QArray");
       render("[");
       sh(_qarray.listqryelem_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryGrndLit foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr _qstr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QStr) foo;
       render("(");
       render("QStr");
       sh(_qstr.string_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum _qnum = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNum) foo;
       render("(");
       render("QNum");
       sh(_qnum.qrynum_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool _qbool = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QBool) foo;
       render("(");
       render("QBool");
       sh(_qbool.qrybool_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul _qnul = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QNul) foo;
       render("QNul");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryBool foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru _qtru = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QTru) foo;
       render("QTru");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal _qfal = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QFal) foo;
       render("QFal");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.QryNum foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt _qint = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QInt) foo;
       render("(");
       render("QInt");
       sh(_qint.integer_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl _qdbl = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.QDbl) foo;
       render("(");
       render("QDbl");
       sh(_qdbl.double_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URI foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI _tokenuri = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.TokenURI) foo;
       render("(");
       render("TokenURI");
       sh(_tokenuri.primuri_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI _basicuri = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.BasicURI) foo;
       render("(");
       render("BasicURI");
       sh(_basicuri.urischeme_);
       sh(_basicuri.uripath_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI _nulluri = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.NullURI) foo;
       render("NullURI");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPath foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath _locatedtedpath = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.LocatedtedPath) foo;
       render("(");
       render("LocatedtedPath");
       sh(_locatedtedpath.urilocation_);
       sh(_locatedtedpath.urirelativepath_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath _relativepath = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.RelativePath) foo;
       render("(");
       render("RelativePath");
       sh(_relativepath.urirelativepath_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URILocation foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation _urinetlocation = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URINetLocation) foo;
       render("(");
       render("URINetLocation");
       sh(_urinetlocation.uriroot_);
       sh(_urinetlocation.urirsrclocation_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLocation foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc _urirsrcportloc = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcPortLoc) foo;
       render("(");
       render("URIRsrcPortLoc");
       sh(_urirsrcportloc.netlocation_);
       sh(_urirsrcportloc.port_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc _urirsrcloc = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRsrcLoc) foo;
       render("(");
       render("URIRsrcLoc");
       sh(_urirsrcloc.netlocation_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRelativePath foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath _slashpath = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.SlashPath) foo;
       render("(");
       render("SlashPath");
       sh(_slashpath.uriroot_);
       render("[");
       sh(_slashpath.listuripathelement_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIRoot foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin _uriorigin = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIOrigin) foo;
       render("URIOrigin");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.NetLocation foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr _dnsaddr = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSAddr) foo;
       render("(");
       render("DNSAddr");
       render("[");
       sh(_dnsaddr.listdnselement_);
       render("]");
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIScheme foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme _atomscheme = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomScheme) foo;
       render("(");
       render("AtomScheme");
       sh(_atomscheme.lident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.URIPathElement foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement _atompathelement = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPathElement) foo;
       render("(");
       render("AtomPathElement");
       sh(_atompathelement.lident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.DNSElement foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement _atomdnselement = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomDNSElement) foo;
       render("(");
       render("AtomDNSElement");
       sh(_atomdnselement.lident_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.Port foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort _atomport = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.AtomPort) foo;
       render("(");
       render("AtomPort");
       sh(_atomport.integer_);
       render(")");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.UUID foo)
  {
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID _kvdbluuid = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBLUUID) foo;
       render("(");
       render("KVDBLUUID");
       sh(_kvdbluuid.lident_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID _kvdbprimuuid = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBPrimUUID) foo;
       render("(");
       render("KVDBPrimUUID");
       sh(_kvdbprimuuid.primuuid_);
       render(")");
    }
    if (foo instanceof com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID)
    {
       com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID _kvdbnulluuid = (com.biosimilarity.lift.lib.kvdbJSON.Absyn.KVDBNullUUID) foo;
       render("KVDBNullUUID");
    }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListQryElem foo)
  {
     for (java.util.Iterator<QryElem> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListSubstPair foo)
  {
     for (java.util.Iterator<SubstPair> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListURIPathElement foo)
  {
     for (java.util.Iterator<URIPathElement> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(com.biosimilarity.lift.lib.kvdbJSON.Absyn.ListDNSElement foo)
  {
     for (java.util.Iterator<DNSElement> it = foo.iterator(); it.hasNext();)
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

