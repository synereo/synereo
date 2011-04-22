// -*- mode: Scala;-*- 
// Filename:    CnxnUtilities.scala 
// Authors:     lgm                                                    
// Creation:    Fri Mar 18 15:15:27 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib._

object CnxnLeafAndBranch
extends CnxnXQuery[String,String,String]
 with CnxnXML[String,String,String]
 with CnxnCtxtInjector[String,String,String]
 with Blobify
 with UUIDOps
{
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

  val aLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	"a"
      )
    )
  val bLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	"b"
      )
    )
  
  val cLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	"c"
      )
    )
  val dLabel =
    new CnxnCtxtLeaf[String,String,String](
      Left(
	"d"
      )
    )        
}
