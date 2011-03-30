package com.imaginej

package monad

package instances

import monad.MonadModule

object IdentityMonadObject
 extends MonadModule {
 type C[+X] = IdentityMonad[X]
 type In[-X] = Unit
 type Out[+X] = X
 override def _return[Y] =
  IdentityMonad[Y](_)
 case class IdentityMonad[+X](protected val open: X)
  extends Monad[X] {
  override def run[W >: X](u: Unit): W = open
  override def flatMap[Y](x_f_cy: X => C[Y]) =
   x_f_cy(this.open)
 }
}

