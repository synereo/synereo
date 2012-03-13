// -*- mode: Scala;-*- 
// Filename:    Concrete.scala<2> 
// Authors:     lgm                                                    
// Creation:    Mon Mar 12 11:46:44 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.json

import com.biosimilarity.lift.lib.zipper._

import scala.xml._
import scala.util.parsing.combinator._
import scala.collection.SeqProxy

trait SimpleJSON
extends JavaTokenParsers {
  def JSONObject : Parser[Any] = "{"~rep( JSONPair )~"}"
  def JSONPair : Parser[Any] = stringLiteral~":"~JSONValue
  def JSONArray : Parser[Any] = "["~repsep( JSONValue, "," )~"]"
  def JSONValue : Parser[Any] = (
    JSONObject
    | JSONArray
    | stringLiteral
    | floatingPointNumber
    | "true"
    | "false"
    | "null"    
  )     
}

object SimpleJSONParser extends SimpleJSON
