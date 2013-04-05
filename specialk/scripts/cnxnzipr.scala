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
  import com.biosimilarity.lift.lib.term.conversion.usage._
  val loc1 = Location[Either[String,String]]( RAFQry1, Top() )
  val z1 =
    new CnxnNavigation[String,String,String] with CnxnMutation[String,String,String] {
  }
  val zipr1 = CnxnStrZipr

  val ttccl =
    new TermToCCLStr()
  val ccbR =
    new CnxnCtxtBranch[String,String,String]( "root", Nil )
  val locR1 =
    Location[Either[String,String]]( ccbR, Top() )
  val cclA1 =
    new CnxnCtxtBranch[String,String,String]( "a", Nil )

  def doParseOne() = {
    ttccl.strToTerm( "a( 1 )" )
  }
}
