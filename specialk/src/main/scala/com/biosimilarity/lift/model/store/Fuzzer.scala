// -*- mode: Scala;-*- 
// Filename:    Fuzzer.scala 
// Authors:     lgm                                                    
// Creation:    Mon Aug 26 15:23:43 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import scala.util.continuations._ 
import scala.collection.mutable.HashMap

import java.util.UUID

package fuzzer {
  trait FuzzyStreams {
    def uuidTuple( l : Int ) : List[UUID] = {
      ( List[UUID]( ) /: ( 1 to l ) )( 
        {
          ( acc, e ) => acc ++ List( UUID.randomUUID )
        }
      )
    }
    def tStream[T]( seed : T )( fresh : T => T ) : Stream[T] = {
      lazy val loopStrm : Stream[T] =
        ( List( seed ) ).toStream append ( loopStrm map fresh );
      loopStrm
    }      
    def uuidStream() : Stream[UUID] =
      tStream[UUID]( UUID.randomUUID )(
        {
          ( uuid : UUID ) => {
            UUID.randomUUID
          }
        }
      )
    def mkUuidStreamStream() : Stream[Stream[UUID]] =
      tStream[Stream[UUID]]( uuidStream() )(
        {
          ( uuidStrm : Stream[UUID] ) => {
            uuidStream()
          }
        }
      )
    @transient
    lazy val uuidStreamStream : Stream[Stream[UUID]] =
      mkUuidStreamStream()  
  }

  trait FuzzyTerms {
    self : FuzzyStreams =>
      def randomGroundTerm(
        truncate : Int = 5,
        prefix : String = "aString",
        rndm : scala.util.Random = new scala.util.Random()
      ) : String = {
        val termType = rndm.nextInt( 3 )
        termType match {
          case 0 => ( rndm.nextInt( 2 ) > 0 ).toString
          case 1 => rndm.nextInt( Int.MaxValue ).toString
          //case 2 => rndm.nextInt( Int.MaxValue ).toFloat.toString
          case 2 => "\"" + prefix + UUID.randomUUID().toString.replace( "-", "" ).substring( 0, truncate ) + "\""
        }
      }
    
    def randomLabelStr(
      uuidStrm : Stream[UUID] = uuidStream(),
      prefix : String = "label",
      maxBredth : Int = 2,
      maxDepth : Int = 2,
      truncate : Int = 10,
      streamPrefix : Int = 1000
    ) : String = {
      val rndm = new scala.util.Random()
      if ( maxBredth > 0 ) {        
        val bredth = rndm.nextInt( maxBredth ) + 1
        val functorLocation = rndm.nextInt( streamPrefix )
        val functor = prefix + uuidStrm( functorLocation ).toString.replace( "-", "" ).substring( 0, truncate )
        val subterms =
          if ( bredth > 1 ) {
            if ( maxDepth > 0 ) {
              ( randomLabelStr( uuidStrm, prefix, maxBredth - 1, maxDepth - 1 ).toString /: ( 2 to bredth ) )(
                {
                  ( acc, e ) => {
                    acc + "," + randomLabelStr( uuidStrm, prefix, maxBredth - 1, maxDepth - 1 ).toString
                  }
                }
              )
            }
            else {
              ( randomGroundTerm( truncate, "aString", rndm ) /: ( 2 to bredth ) )(
                {
                  ( acc, e ) => {
                    acc + "," + randomGroundTerm( truncate, "aString", rndm )
                  }
                }
              )
            }               
          }
            else {
              randomLabelStr( uuidStrm, prefix, maxBredth - 1, maxDepth - 1 ).toString
            }
        functor + "(" + subterms + ")"
      } else {
        randomGroundTerm( truncate, "aString", rndm )
      }
    }  
  }

  trait FuzzyTermStreams {
    self : CnxnString[String,String,String] with FuzzyTerms with FuzzyStreams =>
      def mkRandomLabelStringStream(    
        prefix : String = "label",
        maxLabelDepth : Int = 2,
        uuidStrmStrm : Stream[Stream[UUID]] = mkUuidStreamStream()
      ) : Stream[String] = {
        uuidStrmStrm.map( randomLabelStr( _, prefix, maxLabelDepth ) )
      }
    def mkRandomLabelStream() : Stream[CnxnCtxtLabel[String,String,String]] = {
      mkRandomLabelStringStream().map(
        fromTermString( _ ).getOrElse(
          throw new Exception( "unable to parse label string" )
        )
      )
    }
    @transient
    lazy val randomLabelStream : Stream[CnxnCtxtLabel[String,String,String]] = {
      mkRandomLabelStream()
    }
  }
}


