package com.imaginej

package monad

import computation.ComputationModule

trait MonadModule
 extends ComputationModule {
 type C[+X] <: Monad[X]
 def _return[Y]: Y => C[Y]
 trait Monad[+X]
  extends Computation[X] { self: C[X] =>
  def flatMap[Y](x_f_cy: X => C[Y]): C[Y]
  def map[Y](xfy: X => Y) =
   this flatMap (xfy andThen _return)
 }
}

import instances.IdentityMonadObject

object MonadModule {
 implicit def implicitIdentityMonadObject: IdentityMonadObject.type =
  IdentityMonadObject
}

