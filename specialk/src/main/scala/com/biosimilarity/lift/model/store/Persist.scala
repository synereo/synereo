package com.biosimilarity.lift.model.store

import scala.xml._

/* User: jklassen
*/

trait Persist[OpenReturn,Rslt]
{
  def checkIfDBExists(collectionName: String, leaveOpen: Boolean): Boolean

  def open(collectionName: String ) : OpenReturn

  def drop(collectionName: String) : Unit

  def insertUpdate( recordType : String )(collectionName: String, key: String, value: String) : Unit

  def delete( recordType : String )(collectionName: String, key: String) : Unit

  //def execute(query: String): Unit
  def execute(collectionName : String, query: String): Unit

  //def execute(queries: List[String]): Unit
  def execute(collectionName : String, queries: List[String]): Unit

  //def executeScalar(query: String): String
  def executeScalar(collectionName : String, query: String): String

  // BUGBUG -- lgm: this implies that Elem is our in-memory
  // representation for return values. Should this decision be revisited?
   //commenting out those methods which do not include a collectionName from the interface (for now)
  //def executeWithResults(query: String): List[Elem]
  def executeWithResults( collectionName : String, query : String ) : List[Rslt]

  //def executeWithResults(queries: List[String]): List[Elem]
  def executeWithResults( collectionName : String, queries : List[String] ) : List[Rslt]

  //def count
}
