// -*- mode: Scala;-*- 
// Filename:    ConfigTramp.scala 
// Authors:     lgm                                                    
// Creation:    Mon Mar 28 13:51:38 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib

import net.lag.configgy._

import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

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
  var _configurationFromFile : Option[Map[String,String]] = None
  def configurationFromFile : Map[String,String] = {
    _configurationFromFile match {
      case Some( env ) => env
      case None => {
	val env = processConfigurationFile
	_configurationFromFile = Some( env )
	env
      }
    }
  }

  def processConfigurationFile(
    confFileName : String,
    env : Map[String,String]
  ) : Map[String,String] = {
    Configgy.configure( confFileName )
    val config = Configgy.config    

    for(
      m <- configurationDefaults.getClass.getMethods;      
      if (! builtins.contains( m.getName ) )
    ) {
      val confVal =
	config.getString( m.getName ).getOrElse(
	  m.invoke( configurationDefaults )
	)
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

