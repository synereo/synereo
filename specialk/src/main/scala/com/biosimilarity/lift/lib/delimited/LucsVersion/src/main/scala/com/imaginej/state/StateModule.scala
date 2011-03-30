package com.imaginej

package state

trait StateModule {
 type S
 type C[+X] <: State[X]
 def _get: Unit => C[S]
 def _set: S => C[Unit]
 trait State[+X] { self: C[X] =>
 }
}
