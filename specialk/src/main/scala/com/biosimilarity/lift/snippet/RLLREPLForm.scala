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
import com.biosimilarity.lift.model.RLLEvaluationServiceProxy
import com.biosimilarity.lift.model.RLLEvaluationService

import scala.xml._

class RLLREPLForm {
  import com.biosimilarity.lift.model.store._
  import usage._
  import PersistedMonadicTS._

  lazy val theExchange =
    ptToPt( "SDEC", "localhost", "localhost" )

  lazy val theEvaluationServer =
    new RLLEvaluationService( theExchange )
  
  lazy val theEvaluationClient =
    new RLLEvaluationServiceProxy( theExchange )
    
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
	      (
		"parse "
		+ theREPL.showClientRequestParseTree( paramStr ).toString
	      )
	    )
	  }
	  case JsonCmd( "evaluate", _, paramStr :  String, _ ) => {
	    Text( ("evaluate " + theREPL.eval( paramStr ) ) )
	  }
	  case JsonCmd( "evaluateRemote", _, paramStr :  String, _ ) => {
	    var rslt : Option[Object] = None
	    theEvaluationServer.evalVM
	    theEvaluationClient.evalRemoteVM(
	      paramStr,
	      ( x ) => {
		println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		println( "received rslt: " + x )
		println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		x match {
		  case Some( mTT.Ground( nf ) ) => {
		    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		    println( "rslt: " + nf )
		    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		    rslt = Some( nf )
		  }
		  case Some( mTT.RBound( Some( mTT.Ground( nf ) ), _ ) ) => {
		    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		    println( "rslt: " + nf )
		    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		    rslt = Some( nf )
		  }
		  case _ => {
		    println( "waiting for session response..." )
		  }
		}
		
		println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		println( "unblocking " )
		println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
		// We alert the filter that it can now proceed...
		synchronized {
		  this.notifyAll
		}		
	      }
	    )

	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    println( "blocking " )
	    println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" )
	    // We cannot return until we know that
	    // we have processed the response
	    synchronized {
	      this.wait
	    }

	    Text( ("evaluate remote " + rslt) )
	  }
	  case JsonCmd( "type", _, paramStr :  String, _ ) => {
	    Text( ("type " + "TBD") )
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
