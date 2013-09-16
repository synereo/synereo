// -*- mode: Scala;-*- 
// Filename:    ConfigTramp.scala 
// Authors:     lgm                                                    
// Creation:    Mon Mar 28 13:51:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

//import net.lag.configgy._
import com.typesafe.config._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import java.io.File

trait ConfigurationTrampolineDefaults {
  type ConfigurationDefaults <: java.lang.Object

  def configurationDefaults : ConfigurationDefaults
}

object NoDefaults 

trait ConfigurationTrampoline 
extends ConfigurationTrampolineDefaults {
  type ConfigurationDefaults = java.lang.Object
  def configurationDefaults : ConfigurationDefaults = {
    NoDefaults.asInstanceOf[ConfigurationDefaults]
  }
  
  lazy val _confEnv : Map[String,String] =
    new HashMap[String,String]()
  def confEnv : Map[String,String] = _confEnv
  def configFileName : Option[String]

  def bail () : String = {
    throw new Exception( "value required" );
    null
  }
  val builtins =
    (new java.lang.Object()).getClass.getMethods.toList.map( _.getName )

  // Another way?
  lazy val configurationFromFile : Map[String,String] =
    processConfigurationFile

  def processConfigurationFile(
    confFileName : String,
    env : Map[String,String]
  ) : Map[String,String] = {
    //Configgy.configure( confFileName )
    //val config = Configgy.config    

    val config =
      ConfigFactory.load( ConfigFactory.parseFile( new File( confFileName ) ) )

    for(
      m <- configurationDefaults.getClass.getMethods;      
      if (! builtins.contains( m.getName ) )
    ) {
      val confVal =
	try {
	  config.getString( m.getName )	  
	}
	catch {
	  case e => m.invoke( configurationDefaults )
	}
      env += (( m.getName, confVal.toString ) )
    }

    env
  }
  
  def processConfigurationFile(
    confFileName : String
  ) : Map[String,String] = {
    processConfigurationFile(
      confFileName,
      confEnv
    )
  }
  
  def processConfigurationFile() : Map[String,String] = {
    val env : Map[String,String] = confEnv
    configFileName match {
      case Some( cfn : String ) =>
	processConfigurationFile( cfn, env )
      case None => {	
	for(
	  m <- configurationDefaults.getClass.getMethods;
	  if (! builtins.contains( m.getName ) )
	) {
	  val confVal =
	    m.invoke( configurationDefaults )
	  env += (( m.getName, confVal.toString ) )
	}
      }
    }
    env
  }
}

