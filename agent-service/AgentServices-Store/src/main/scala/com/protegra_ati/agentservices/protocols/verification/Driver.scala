// -*- mode: Scala;-*- 
// Filename:    Driver.scala 
// Authors:     lgm                                                    
// Creation:    Thu Feb 13 16:40:06 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.{PortableAgentCnxn, PortableAgentBiCnxn}
import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
import com.protegra_ati.agentservices.protocols.msgs._
import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.lib._
import scala.util.continuations._
import java.util.UUID

package usage {
  import com.biosimilarity.evaluator.distribution.FuzzyStreams
  object VerificationDriver extends FuzzyStreams {
    import com.biosimilarity.evaluator.distribution.diesel.EvalNodeMapper
    import com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor
    import com.biosimilarity.evaluator.distribution.diesel.DieselEngineCtor.StdEvalChannel
    def nextNode(
    ) : ( String, StdEvalChannel ) = {
      val keyNodePair@( dslNodeKey, dslNode ) =
        ( UUID.randomUUID( ).toString -> DieselEngineCtor.dslEvaluatorAgent[PersistedKVDBNodeRequest,PersistedKVDBNodeResponse]( ) )
      EvalNodeMapper += keyNodePair
      keyNodePair
    }
    def nodeStream(
    ) : Stream[StdEvalChannel] = {
      val ( _, node ) = nextNode
      tStream( node )(
        { node => { val ( _, node ) = nextNode; node } }
      )
    }    
  }
}
