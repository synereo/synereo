package com.imaginej

package monad

package state.instances

import monad.instances.IdentityMonadObject
import monad.state.transformed.StateTransformedMonadModule

abstract class StateTransformedIdentityMonadModule(implicit override val monadObject: IdentityMonadObject.type )
 extends StateTransformedMonadModule {
 type MonadModuleType = IdentityMonadObject.type
 type C[+X] = StateTransformedIdentityMonad[X]
 type Out[+X] = (X, S)
 protected[monad] def _closeT[Y] =
  StateTransformedIdentityMonad[Y](_)
 case class StateTransformedIdentityMonad[+X](protected[monad] val openT: S => IdentityMonadObject.C[(X, S)])
  extends StateTransformedMonadTrait[X] {
  def run[W >: X](s: S): (W, S) =
   runST(s)(())
 }
}

import monad.MonadModule.implicitIdentityMonadObject

object IntStateTransformedIdentityMonadObject
 extends StateTransformedIdentityMonadModule {
 type S = Int
}

