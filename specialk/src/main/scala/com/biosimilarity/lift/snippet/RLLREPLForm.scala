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

import com.biosimilarity.lift.model.RLLREPL

import scala.xml._

class RLLREPLForm {
  val theREPL = new RLLREPL()
  var theTerm : String = "*"
  var theClientRequestStr : String = evalStr()

  def evalStr() = theTerm
  def clientRequestRequest() = theClientRequestStr

  object json extends JsonHandler {          
    def apply( jsonCmd : Any ) : JsCmd = {
      SetHtml(
	"result",
	jsonCmd match {
	  case JsonCmd( "parse", _, paramStr : String, _ ) => {
	    Text(
	      ("parse " + theREPL.showClientRequestParseTree(paramStr).toString) )
	  }
	  case JsonCmd( "evaluate", _, paramStr :  String, _ ) => {
	    Text(
	      ("evaluate " + theREPL.eval(paramStr)) )
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
      println( "parsed : " + theParseResponseStr );
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
    println("updateWhat with " + str)
    JsCmds.SetHtml("parseTree", whatNode(str))
  }

  def show(xhtml: NodeSeq): NodeSeq = {
    println( "entering show with " + xhtml.toString )
    bind(
      "json", xhtml,
      "script" -> Script( json.jsCmd ),
      AttrBindParam(
	"onclick",
	Text(
	  json.call(
	    ElemById( "json_verb" )~>Value,
	    (ElemById( "expression" )~>Value)
	  ).toJsCmd
	),
	"onclick"
	)
    )
  }
}
