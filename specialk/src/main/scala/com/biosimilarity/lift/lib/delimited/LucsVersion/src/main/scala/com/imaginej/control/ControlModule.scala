package com.imaginej

package control

trait ControlModule {
 type A
 type B[C[_], X]
 trait CCV[B[C[_], X], C[_], +X]
 case class Iru[B[C[_], X], C[_], +X](x: X) extends CCV[B, C, X]
 case class Deru[B[C[_], X], C[_], X](
  sc: SC[B, C, A, X],
  body: B[C, A]) extends CCV[B, C, X]
 type SC[B[C[_], _], C[_], X, Y] = C[X] => C[Y]
 type MC[B[C[_], _], C[_], X, Y] = SC[B, C, X, Y] => C[Y]
 trait Prompt[B[C[_], _], C[_], X]
  extends (MC[B, C, A, X] => B[C, A], B[C, A] => Option[MC[B, C, A, X]])
 type C[+X] <: Control[X]
 def _withSubCont[W]: Prompt[B, C, W] => MC[B, C, A, W] => C[A]
 trait Control[+X] { self: C[X] =>
  def pushSubCont[V >: X, Y, W >: Y](sc: SC[B, C, V, W]): C[W]
  def pushPrompt[W >: X](p: Prompt[B, C, W]): C[W]
 }
}
