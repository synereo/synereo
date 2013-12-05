// -*- mode: Scala;-*- 
// Filename:    IntroPrefixRaceTest.scala 
// Authors:     lgm                                                    
// Creation:    Thu Dec  5 12:44:17 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.protegra_ati.agentservices.protocols

import com.biosimilarity.evaluator.distribution.FuzzyStreams

// n
// I2A
// A2I

object NodeSupply extends FuzzyStreams with Serializable {
  import com.biosimilarity.evaluator.distribution.diesel._
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._
  def nodeStream(
  ) : Stream[Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]] = {
    tStream[Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse]](
      DieselEngineCtor.dslEvaluatorAgent( )
    )(
      {
        seed => DieselEngineCtor.dslEvaluatorAgent( )
      }
    )
  }

  def keyNodeStream(
  ) : Stream[( UUID, Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse] )] = {
    uuidStream.zip( nodeStream )
  }

  def mkNodeSupply( initialCut : Int = 10 ) = {
    for( ( uuid, node ) <- keyNodeStream.take( initialCut ) ) {
      EvalNodeMapper += ( uuid.toString -> node )
    }
  }
}

object TestIntroRaceOne extends FuzzyStreams with Serializable {
  import com.biosimilarity.evaluator.distribution.diesel._
  import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
  import com.protegra_ati.agentservices.protocols.msgs.ProtocolMessage
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._    

  def testInitiatorPrefix(
    n : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    aGetIntroProfileRq : ProtocolMessage,
    aGetIntroProfileRspLabel : CnxnCtxtLabel[String,String,String]
  ) = {
    n.put( I2A )(
      aGetIntroProfileRq.toLabel,
      mTT.Ground( PostedExpr( aGetIntroProfileRq ) )
    ) 
    for( e <- n.get( A2I )( aGetIntroProfileRspLabel ) ) {
      println( "here" )
    }
  }

  def testRecipientPrefix(
    n : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    getIntroProfileRqLabel : CnxnCtxtLabel[String,String,String],
    getIntroProfileRsp : ProtocolMessage
  ) = {
    for( e <- n.get( I2A )( getIntroProfileRqLabel ) ) {
      n.put( A2I )(
        getIntroProfileRsp.toLabel,
        mTT.Ground( PostedExpr( getIntroProfileRsp ) )
      )
    }
  }

  def race(
    n: Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    getIntroProfileRq : ProtocolMessage,
    getIntroProfileRsp : ProtocolMessage
  )( waitingPeriod : Int = 1000 ) = {
    spawn {
      testRecipientPrefix( n, I2A, A2I )(
        getIntroProfileRq.toLabel,
        getIntroProfileRsp
      )
    }
    Thread.sleep( waitingPeriod )
    spawn {
      testInitiatorPrefix( n, I2A, A2I )(
        getIntroProfileRq.toLabel,
        getIntroProfileRsp
      )
    }    
  }
  
  def race(
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    getIntroProfileRq : ProtocolMessage,
    getIntroProfileRsp : ProtocolMessage
  )( waitingPeriod : Int = 1000 ) = {
    NodeSupply.mkNodeSupply( 1 )
    race(
      EvalNodeMapper( EvalNodeMapper.keys.toList( 0 ) ),
      I2A, A2I,
      getIntroProfileRq, getIntroProfileRsp
    )
  }
}

// ---

// nI2A
// nA2I

// I2A
// A2I

object TestIntroRaceTwo {
  import com.biosimilarity.evaluator.distribution.diesel._
  import com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr
  import com.protegra_ati.agentservices.protocols.msgs.ProtocolMessage
  import com.biosimilarity.evaluator.distribution.diesel.DieselEngineScope._    

  def testInitiatorPrefix(
    nI2A : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    nA2I : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    aGetIntroProfileRq : ProtocolMessage,
    aGetIntroProfileRspLabel : CnxnCtxtLabel[String,String,String]
  ) = {
    nI2A.put( I2A )(
      aGetIntroProfileRq.toLabel,
      mTT.Ground( PostedExpr( aGetIntroProfileRq ) )
    ) 
    for( e <- nA2I.get( A2I )( aGetIntroProfileRspLabel ) ) {
      println( "here" )
    }
  }

  def testRecipientPrefix(
    nI2A : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    nA2I : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    getIntroProfileRqLabel : CnxnCtxtLabel[String,String,String],
    getIntroProfileRsp : ProtocolMessage
  ) = {
    for( e <- nI2A.get( I2A )( getIntroProfileRqLabel ) ) {
      nA2I.put( A2I )(
        getIntroProfileRsp.toLabel,
        mTT.Ground( PostedExpr( getIntroProfileRsp ) )
      )
    }
  }

  def race(
    nI2A : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    nA2I : Being.AgentKVDBNode[PersistedKVDBNodeRequest, PersistedKVDBNodeResponse],
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    getIntroProfileRq : ProtocolMessage,
    getIntroProfileRsp : ProtocolMessage
  )( waitingPeriod : Int = 1000 ) = {
    spawn {
      testRecipientPrefix( nI2A, nA2I, I2A, A2I )(
        getIntroProfileRq.toLabel,
        getIntroProfileRsp
      )
    }
    Thread.sleep( waitingPeriod )
    spawn {
      testInitiatorPrefix( nA2I, nI2A, I2A, A2I )(
        getIntroProfileRq.toLabel,
        getIntroProfileRsp
      )
    }    
  }
  
  def race(
    I2A : PortableAgentCnxn,
    A2I : PortableAgentCnxn
  )(
    getIntroProfileRq : ProtocolMessage,
    getIntroProfileRsp : ProtocolMessage
  )( waitingPeriod : Int = 1000 ) = {
    NodeSupply.mkNodeSupply( 2 )
    val nodeKeys = EvalNodeMapper.keys.toList
    race(
      EvalNodeMapper( nodeKeys( 0 ) ),
      EvalNodeMapper( nodeKeys( 1 ) ),
      I2A, A2I,
      getIntroProfileRq, getIntroProfileRsp
    )
  }
}
