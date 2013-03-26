package com.biosimilarity.lift.model.store

import org.prolog4j.{ProverFactory, Prover}

object LocalProverFactoryPool {
  import org.apache.commons.pool.BasePoolableObjectFactory
  import org.apache.commons.pool.impl.GenericObjectPool

  @transient
  lazy val _prover = ProverFactory.getProver()

  case class LocalProverFactory() extends BasePoolableObjectFactory[Prover] {
    override def makeObject() : Prover = synchronized {
      ProverFactory.getProver()
    }
  }

  // No max # of objects in pool, no max # of idle objects in pool
  @transient
  lazy val localProverFactory = new GenericObjectPool[Prover]( LocalProverFactory(), -1, GenericObjectPool.WHEN_EXHAUSTED_GROW, -1, -1 )

  def getProver() = {
    try {
      localProverFactory.borrowObject()
    }
    catch {
      case e : java.lang.IllegalStateException => {
        //ProverFactory.getProver()
        _prover
      }
    }
  }

  def dropProver( prover : Prover ) = {
    if ( prover != _prover ) {
      localProverFactory.returnObject( prover )
    }
  }
}
