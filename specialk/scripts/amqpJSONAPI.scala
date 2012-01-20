// -*- mode: Scala;-*- 
// Filename:    amqpJSONAPI.scala 
// Authors:     lgm                                                    
// Creation:    Thu Jan 19 15:17:52 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.lib.kvdbJSON._
import com.biosimilarity.lift.lib.kvdbJSON.Absyn._

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.lib._

import java.net.URI
import java.util.UUID
import java.io.StringReader

trait REPL[Lexer,Parser,AST] {
  def lexer ( str : String ) : Lexer
  def parser ( str : String ) : Parser
  def read( str : String ) : AST
  def print( msg : AST ) : String
}

object REPLUseCase extends REPL[Yylex,parser,Message] with UUIDOps {
  //lazy val to = new URI( "agent", "localhost", "/connect", "" )
  lazy val to = "agent://localhost/connect"
  lazy val nto = "null"
  //lazy val from = new URI( "agent", "localhost", "/connect", "" )
  lazy val from = "agent://localhost/connect"
  lazy val nfrom = "null"
  //lazy val msgId = "AAAAAA-BBBBBB-CCCCCC-DDDDDD-EEEEEE-FFFFFF"
  lazy val msgId : String = getUUID + ""
  //lazy val msgId = "thisShouldBeAUUID"
  lazy val nmsgId = "null"
  //lazy val flowId = "FFFFFF-EEEEEE-DDDDDD-CCCCCC-BBBBBB-AAAAAA"
  lazy val flowId : String = getUUID + ""
  //lazy val flowId = "thisShouldAlsoBeAUUID"
  lazy val nflowId = "null"
  val msgBody = 
    (
      "{ "
      + "\"getRequest\" : "
      + "{ "
      +    "\"ask\" : "
      +        "{ " 
      +           "\"node\" : "
      +              "[" 
      +                "{ "
      +                   "\"machine\" : "
      +                      "["
      +                        "\"sl390\""
      +                      "]"
      +                " }, "
      +                "{ "
      +                   "\"os\" : " 
      +                      "["
      +                        "\"Ubuntu\","
      +                        "\"11.04\""
      +                      "]"
      +                " }"
      +              " ]"
      +        " }"
      + " }"
      + " }"
    );
  lazy val emptyBody = 
    "[" + "{" + "\"testReqEmptyBody\"" + ":" + "null" + "}" + "]"
  lazy val justification = 
    "{ " + "\"response\"" + " : " + "null" + " }"
  lazy val emptyHdrs =
    "[" + "{" + "\"testReqEmptyHdrs\"" + ":" + "null" + "}" + "]"
  lazy val allHdrs = 
    (
      "["
      + to + "," + from + "," + msgId + "," + flowId + ","
      + justification
      + "]"
    );
  lazy val nURIHdrs =
    (
      "["
      + nto + "," + nfrom + "," + msgId + "," + flowId + "," 
      + justification
      + "]"
    );
  lazy val nUUIDHdrs =
    (
      "["
      + to + "," + from + "," + nmsgId + "," + nflowId + "," 
      + justification
      + "]"
    );
  lazy val allNullHdrs = 
    (
      "["
      + nto + "," + nfrom + "," + nmsgId + "," + nflowId + "," 
      + justification
      + "]"
    );
  lazy val msgHdrsBody = 
    makeMsg( allHdrs, msgBody );
  lazy val msgHdrsNoBody = 
    makeMsg( allHdrs, emptyBody );  
  lazy val msgAllNullHdrsBody =     
    makeMsg( allNullHdrs, msgBody );
  lazy val msgAllNullHdrsNoBody =     
    makeMsg( allNullHdrs, emptyBody );
  lazy val msgNoHdrsBody =     
    makeMsg( emptyHdrs, msgBody );
  lazy val msgNoHdrsNoBody =     
    makeMsg( emptyHdrs, emptyBody );

  def makeMsg( hdrs : String, body : String ) : String = {
    (
      "{ "
      + "\"headers\" : "
      + hdrs
      + ", "
      + "\"body\" : "
      + body
      + " }"
    )
  }

  override def lexer ( str : String ) : Yylex = new Yylex( new StringReader( str ) )
  override def parser ( str : String ) : parser = new parser( lexer( str ) )
  override def read( str : String ) : Message = (parser( str )).pMessage()
  override def print( msg : Message ) : String = PrettyPrinter.show( msg )
  
  def readAMessage( msg : String ) = {
    println( "Reading message: " + "\n" + msg )
    print( read( msg ) )
  }
}
