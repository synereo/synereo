// -*- mode: Scala;-*- 
// Filename:    BFactoryConfigSetup.scala 
// Authors:     lgm                                                    
// Creation:    Thu Nov 21 10:30:06 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.xml._
//import com.biosimilarity.lift.model.store.mongo._
import com.biosimilarity.lift.model.agent._
import com.biosimilarity.lift.model.msg._
import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.lib.moniker._
import net.liftweb.amqp._

import scala.util.continuations._ 
import scala.concurrent.{Channel => Chan, _}
//import scala.concurrent.cpsops._
import scala.xml._
import scala.collection.mutable.Map
import scala.collection.mutable.MapProxy
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.MutableList

//import com.rabbitmq.client._

import org.prolog4j._

import com.mongodb.casbah.Imports._

//import org.json4s._
//import org.json4s.jackson.JsonMethods._
//import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.json.JettisonMappedXmlDriver

import biz.source_code.base64Coder.Base64Coder

import javax.xml.transform.OutputKeys

import java.util.UUID
import java.net.URI
import java.util.Properties
import java.io.File
import java.io.FileInputStream
import java.io.OutputStreamWriter
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import java.io.ObjectOutputStream
import java.io.ByteArrayOutputStream

package bfactory {
  object LabelReader extends CnxnString[String,String,String] {
    def apply(
      lblStr : String
    ) : CnxnCtxtLabel[String,String,String] = {
      fromTermString(
        lblStr
      ).getOrElse( throw new Exception( "unable to parse label string " + lblStr ) )
    }
  }
  object BFactoryMapInitializer extends Serializable {
    import scala.collection.JavaConverters._    
    import com.protegra_ati.agentservices.store.extensions.StringExtensions._

    def makeMap() = {
      for(
        entry <- BFactoryDefaultServiceContext.eServe.evalConfig(
          "eval.client.conf"
        ).getObject( "BFactoryMap" ).entrySet.asScala;
        eKey = entry.getKey;
        eVal = entry.getValue;
        mapObj = eVal.asInstanceOf[com.typesafe.config.ConfigObject];
        mapCfg = mapObj.toConfig;
        cnxnCfg = mapCfg.getObject( "cnxn" ).toConfig;
        cnxnSrc = cnxnCfg.getString( "src" );
        cnxnLabel = cnxnCfg.getString( "label" );
        cnxnTrgt = cnxnCfg.getString( "trgt" );
        cnxn = PortableAgentCnxn( cnxnSrc.toURI, cnxnLabel, cnxnTrgt.toURI );
        labelStr = mapCfg.getString( "label" );
        classStr = mapCfg.getString( "class" )
      ) {      
        BasicLogService.tweet( "mapping " + "(" + cnxn + " , " + labelStr + ")" + " -> " + classStr )
        BFactoryDefaultServiceContext.eServe.bFactoryMgr().mapBehavior(
          cnxn,
          LabelReader( labelStr ),
          classStr,
          { optRsrc => 
            BasicLogService.tweet( "mapped " + "(" + cnxn + " , " + labelStr + ")" + " -> " + classStr )
          }
        )
      }
    }
  }
}
