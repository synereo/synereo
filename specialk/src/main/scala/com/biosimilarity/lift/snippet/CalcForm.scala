package com.biosimilarity.lift.snippet

import net.liftweb._
import http._
import S._
import SHtml._
import util._
import Helpers._
import mapper._
//import textile._
import js.{JE, JsCmd, JsCmds}
import JsCmds._
import JE._

import com.biosimilarity.lift.model.REPL
import com.biosimilarity.lift.lib._

import scala.xml._

class CalcForm 
extends WireTap
with Journalist
with ConfiggyReporting
with ConfiggyJournal {
  override def tap [A] ( fact : A ) : Unit = {
    reportage( fact )
  }

  val theREPL = new REPL()
  var theTerm : String = "{ | }"
  var theClientRequestStr : String = evalStr()

  def evalStr() = theTerm
  def clientRequestRequest() = theClientRequestStr

  object json extends JsonHandler {          
    def apply( jsonCmd : Any ) : JsCmd = {
      reportage( "in apply on JsonHandler object" )
      SetHtml(
	"result",
	jsonCmd match {
	  case JsonCmd( "parse", _, paramStr : String, _ ) => {
	    Text(
	      ("parse " + theREPL.showClientRequestParseTree(paramStr).toString) )
	  }
	  case JsonCmd( "evaluate", _, paramStr :  String, _ ) => {
	    Text(
	      ("evaluate " + paramStr) )
	  }
	  case JsonCmd( "type", _, paramStr :  String, _ ) => {
	    Text(
	      ("type " + "TBD") )
	  }
	}
      )
    }
  }
  
  def whatNode(termStr : String) = {
    theTerm = termStr
    theClientRequestStr = evalStr()
    var theParseResponseStr : String =
      ("failure: " + theClientRequestStr + "\n");
    try {
      theParseResponseStr =
	theREPL.showClientRequestParseTree(theClientRequestStr)
      reportage( "parsed : " + theParseResponseStr );
	//theREPL.readEvalPrint(theClientRequestStr)
    }
    catch {
      case e => {
	val sw : java.io.StringWriter =	new java.io.StringWriter( );
	e.printStackTrace( new java.io.PrintWriter( sw, true ) );
	theParseResponseStr = theParseResponseStr + e.toString
      }
    }
    <div id="parseTree">{theParseResponseStr}</div>
  }

  def updateWhat(str: String): JsCmd = {
    reportage("updateWhat with " + str)
    JsCmds.SetHtml("parseTree", whatNode(str))
  }

  def show(xhtml: NodeSeq): NodeSeq = {
    reportage( "entering show with " + xhtml.toString )
    bind(
      "json", xhtml,
      "script" -> Script( json.jsCmd ),
      AttrBindParam(
	"onclick1",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "oneBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick2",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "twoBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick3",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "threeBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclickPlus",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "plusBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick4",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "fourBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick5",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "fiveBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick6",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "sixBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclickMinus",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "minusBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick7",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "sevenBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick8",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "eightBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick9",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "nineBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclickTimes",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "timesBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclickClear",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "clearBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclick0",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "zeroBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclickEval",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "DoItBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	),
      AttrBindParam(
	"onclickDiv",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "divBtn" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	)
    )
  }
}
