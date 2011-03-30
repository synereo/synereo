package com.imaginej

package monad

package state.transformed

import com.imaginej.monad.state.StateMonadModule
import com.imaginej.monad.transformed.TransformedMonadModule

trait StateTransformedMonadModule
 extends TransformedMonadModule
 with StateMonadModule {
 type C[+X] <: StateTransformedMonadTrait[X]
 type ToC[+X] = S => FromC[(X, S)]
 type In[-X] = S
 override def _lift[Y] =
  cy => s =>  
   cy flatMap { y =>
    monadObject._return((y, s))
   }
 override def _get =
  _ =>
   _closeT { s =>
    monadObject._return((s, s))
   }
 override def _set =
  s =>
   _closeT { _ =>
    monadObject._return(((), s))
   }
 trait StateTransformedMonadTrait[+X]
  extends TransformedMonadTrait[X]
  with StateMonad[X] { self: C[X] =>
  override def flatMap[Y](xf_cy: X => C[Y]) =
   _closeT { s =>
    this.openT(s) flatMap {
     case (x, s) =>
      xf_cy(x).openT(s)
    }
   }
  def runST[W >: X](s: S)(i: FromIn[(W, S)]): FromOut[(W, S)] =
   openT(s) run (i)
  }   
 }

