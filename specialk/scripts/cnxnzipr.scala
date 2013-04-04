// -*- mode: Scala;-*- 
// Filename:    cnxnzipr.scala 
// Authors:     lgm                                                    
// Creation:    Wed Apr  3 22:21:03 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

import com.biosimilarity.lift.model.store._
import com.biosimilarity.lift.model.store.usage._
import PersistedMonadicKVDBMongoNet._
import PersistedMongoMolecularUseCase._
import quicktest._
import ExerciseMongo._
import com.biosimilarity.lift.lib.zipper._
import com.biosimilarity.lift.lib.navigation.{Left=>ZLeft, Right=>ZRight, _}
import com.biosimilarity.lift.lib.term.conversion._

object CnxnZipr {
  val loc1 = Location[Either[String,String]]( RAFQry1, Top() )
  val z1 =
    new CnxnNavigation[String,String,String] with CnxnMutation[String,String,String] {
  }
}
