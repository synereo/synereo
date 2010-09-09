// -*- mode: Scala;-*- 
// Filename:    Misc.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jul 28 16:43:33 2010 
// Copyright:   Not supplied 
// Description: Derived from Lift examples
// ------------------------------------------------------------------------

package com.biosimilarity.lift.snippet
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.model.store._
//import com.biosimilarity.lift.lib.ScalaxbFileCompiler
//import com.biosimilarity.lift.lib.BNFCProjectBuilder

import _root_.net.liftweb._
import http._
import mapper._
import S._
import SHtml._

import common._
import util._
import Helpers._

import _root_.scala.xml.{NodeSeq, Text, Group}
import _root_.java.util.Locale

import java.io.{StringWriter}
import scala.collection.{Map, Set}
import scala.collection.mutable.{ListBuffer, ListMap}
import java.io.{File, BufferedReader, Reader, FileReader, Writer, FileWriter}

class Misc
//extends ScalaxbFileCompiler
//with BNFCProjectBuilder
{
  // the request-local variable that hold the file parameter
  private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)

  /**
   * Bind the appropriate XHTML to the form
   */
  def upload(xhtml: Group): NodeSeq = {
    if (S.get_?) {
      bind(
	"ul",
	chooseTemplate("choose", "get", xhtml),
	"file_upload" -> fileUpload(ul => theUpload(Full(ul))))
    }
    else {
      bind(
	"ul",
	chooseTemplate("choose", "post", xhtml),
	"file_name" -> theUpload.is.map(v => Text(v.fileName)),
	"mime_type" -> theUpload.is.map(v => Box.legacyNullTest(v.mimeType).map(Text).openOr(Text("No mime type supplied"))),
	"length" -> theUpload.is.map(v => Text(v.file.length.toString)),
	"md5" -> theUpload.is.map(v => Text(hexEncode(md5(v.file)))),
	"compiled_file" -> theUpload.is.map( v => Text( "to do" ) )
      )
    }
  }
}



