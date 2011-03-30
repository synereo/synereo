package com.imaginej

package computation

trait ComputationModule {
 type C[+X] <: Computation[X]
 type In[-X]
 type Out[+X]
 trait Computation[+X] { self: C[X] =>
  def run[W >: X](i: In[W]): Out[W]
 }
}
