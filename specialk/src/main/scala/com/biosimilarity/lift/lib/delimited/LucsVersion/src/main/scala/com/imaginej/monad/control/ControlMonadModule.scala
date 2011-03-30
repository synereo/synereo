package com.imaginej

package monad

package control

import monad.MonadModule
import com.imaginej.control.ControlModule

trait ControlMonadModule
 extends MonadModule
 with ControlModule {
 type C[+X] <: ControlMonad[X]
 trait ControlMonad[+X]
  extends Monad[X]
  with Control[X] { self: C[X] =>
 }
}
