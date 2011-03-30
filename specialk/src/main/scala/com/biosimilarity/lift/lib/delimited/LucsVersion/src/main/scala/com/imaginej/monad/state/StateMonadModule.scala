package com.imaginej

package monad

package state

import monad.MonadModule
import com.imaginej.state.StateModule

trait StateMonadModule
 extends MonadModule
 with StateModule {
 type C[+X] <: StateMonad[X]
 // utilities
 def _changeAndTransform[T]: (S => S) => (S => T) => C[T] =
  sfs => sft =>
   for {
    oldS <- _get(())
    _ <- _set(sfs(oldS))
    newS <- _get(())
   } yield(sft(newS))
 trait StateMonad[+X]
  extends Monad[X]
  with State[X] { self: C[X] =>
 }
}

import instances.IntStateTransformedIdentityMonadObject

object StateMonadModule {
 implicit def implicitIntStateTransformedIdentityMonadObject: IntStateTransformedIdentityMonadObject.type =
  IntStateTransformedIdentityMonadObject

}
