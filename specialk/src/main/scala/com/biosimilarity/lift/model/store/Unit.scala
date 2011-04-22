// -*- mode: Scala;-*- 
// Filename:    Unit.scala 
// Authors:     lgm                                                    
// Creation:    Wed Sep  8 14:53:43 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib._

import org.prolog4j._

object Exercise
extends CnxnUnificationTermQuery[String,String,String]
with CnxnCtxtInjector[String,String,String]
with CnxnConversions[String,String,String]
with UUIDOps
{
  def prover = getProver()

  val leavesOfConstance =
    List( "a", "b", "c", "d", "e" ).map( new CnxnLeaf[String,String]( _ ) )
  val leavesOfVariation =
    List( "X", "Y", "Z" ).map(
      ( s : String ) => new CnxnCtxtLeaf[String,String,String](	Right( s ) )
    )
  val branch1 =
    new CnxnBranch[String,String](
      "one",
      List(
	leavesOfConstance( 0 ),
	leavesOfConstance( 1 ),
	leavesOfConstance( 2 )
      )
    )
  val branch2 =
    new CnxnBranch[String,String](
      "two",
      List( 
	leavesOfConstance( 3 ),
	leavesOfConstance( 4 )
      )
    )
  val branch3 =
    new CnxnBranch[String,String]( "three", List( branch1, branch2 ) )

  val branch4 = inject( branch1 )
  val branch5 = inject( branch2 )
  val branch6 =
    new CnxnCtxtBranch[String,String,String](
      "two",
      List(
	leavesOfVariation( 0 ),
	leavesOfVariation( 1 )
      )
    )
  val branch7 =
    new CnxnCtxtBranch[String,String,String](
      "seven",
      List( branch4, branch6, branch5 )
    )
  val branch8 =
    new CnxnCtxtBranch[String,String,String](
      "three",
      List( branch4, branch6 )
    )

  object theTSpace extends TermStore[String,String,String,String]
  
  def exchange() = {
    theTSpace.get( branch8 )
    theTSpace.get( branch8 )

    theTSpace.put( branch3, "The magic 8 ball says: 'yes!'" )
    theTSpace.put( branch3, "The magic 8 ball says: 'come back again, later.'" )
  }
}
