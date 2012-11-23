package com.biosimilarity.lift.model.store

import org.prolog4j.{ProverFactory, Prover}

object LocalProverFactoryPool {
  import org.apache.commons.pool.BasePoolableObjectFactory
  import org.apache.commons.pool.impl.GenericObjectPool

  @transient
  lazy val _prover = ProverFactory.getProver()

  case class LocalProverFactory() extends BasePoolableObjectFactory[Prover] {
    override def makeObject() : Prover = {
      ProverFactory.getProver()
    }
  }

  @transient
  lazy val localProverFactory = new GenericObjectPool[Prover]( LocalProverFactory() )

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
