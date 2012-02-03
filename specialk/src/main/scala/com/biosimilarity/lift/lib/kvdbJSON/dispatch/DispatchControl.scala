// -*- mode: Scala;-*- 
// Filename:    DispatchControl.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb  2 13:59:05 2012 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.kvdbJSON

import com.biosimilarity.lift.lib.kvdbJSON.Absyn.{
  URI => kvdbURI, UUID => kvdbUUID, Message => kvdbMessage, _
}

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store._
//import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._

import scala.util.continuations._
import scala.collection.mutable.HashMap
import scala.xml._

import org.prolog4j._
import biz.source_code.base64Coder.Base64Coder

import java.net.URI
import java.io.StringReader
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

trait StandardDispatchController {
  def dispatcher : KVDBJSONAPIDispatcher
  def trgtURI : URI
  def srcURI : URI
  def replyURI : URI
  def setup(
    dispatcher : KVDBJSONAPIDispatcher,
    trgtURI : URI,
    srcURI : URI,
    replyURI : URI
  ) : KVDBJSONAPIDispatcher
  def setup() : KVDBJSONAPIDispatcher = 
    setup( dispatcher, trgtURI, srcURI, replyURI )
}

case class StdDispatchController(
  override val dispatcher : KVDBJSONAPIDispatcher,
  override val trgtURI : URI,
  override val srcURI : URI,
  override val replyURI : URI
) extends StandardDispatchController {
  def setup(
    dispatcher : KVDBJSONAPIDispatcher,
    trgtURI : URI,
    srcURI : URI,
    replyURI : URI
  ) : KVDBJSONAPIDispatcher = {
    // Configure dispatcher to serve requests with a "to" header of trgtURI
    // and a "from" header of srcURI
    // and deposit replies according to the following scheme
    // if replyURI = scheme://host/root
    // replyQueue = root_queue, replyExchange = root_exchange
    
    dispatcher.addSingletonKVDB( trgtURI )
    dispatcher.addReplyURI( srcURI, replyURI )
    dispatcher.serveAPI
    
    dispatcher
  }
}

package usage {  
  object Dispatcher extends StdDispatchController(
    new KVDBJSONAPIDispatcher( new URI( "amqp", "localhost", "/kvdb", "" ) ),
    new URI( "agent", "localhost", "/kvdbDispatchStore1", "" ),
    new URI( "agent", "localhost", "/kvdbDispatchStore2", "" ),
    new URI( "amqp", "localhost", "/kvdbReply", "" )
  ) {    
  }
}

