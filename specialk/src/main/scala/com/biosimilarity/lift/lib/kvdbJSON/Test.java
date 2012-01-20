package com.biosimilarity.lift.lib.kvdbJSON;
import java_cup.runtime.*;
import com.biosimilarity.lift.lib.kvdbJSON.*;
import com.biosimilarity.lift.lib.kvdbJSON.Absyn.*;
import java.io.*;

public class Test
{
  public static void main(String args[]) throws Exception
  {
    Yylex l = null;
    parser p;
    try
    {
      if (args.length == 0) l = new Yylex(System.in);
      else l = new Yylex(new FileReader(args[0]));
    }
    catch(FileNotFoundException e)
    {
     System.err.println("Error: File not found: " + args[0]);
     System.exit(1);
    }
    p = new parser(l);
    /* The default parser is the first-defined entry point. */
    /* You may want to change this. Other options are: */
    /* pLblReqHeader, pLblRspHeader, pLblReqBody, pLblRspBody, pReqHeader, pRspHeader, pKVDBRequest, pKVDBResponse, pAskReqPacket, pAskRspPacket, pTellReqPacket, pTellRspPacket, pReqJust, pRspJust, pAskReq, pTellReq, pAskRsp, pTellRsp, pStatus, pPattern, pBlob, pSubstitution, pSubstPair, pQryTerm, pQryElem, pQryValue, pQryArray, pQryGrndLit, pQryBool, pQryNum, pURI, pURIPath, pURILocation, pURIRsrcLocation, pURIRelativePath, pURIRoot, pNetLocation, pURIScheme, pURIPathElement, pDNSElement, pPort, pUUID, pListQryElem, pListSubstPair, pListURIPathElement, pListDNSElement */
    try
    {
      com.biosimilarity.lift.lib.kvdbJSON.Absyn.Message parse_tree = p.pMessage();
      System.out.println();
      System.out.println("Parse Succesful!");
      System.out.println();
      System.out.println("[Abstract Syntax]");
      System.out.println();
      System.out.println(PrettyPrinter.show(parse_tree));
      System.out.println();
      System.out.println("[Linearized Tree]");
      System.out.println();
      System.out.println(PrettyPrinter.print(parse_tree));
    }
    catch(Throwable e)
    {
      System.err.println("At line " + String.valueOf(l.line_num()) + ", near \"" + l.buff() + "\" :");
      System.err.println("     " + e.getMessage());
      System.exit(1);
    }
  }
}
