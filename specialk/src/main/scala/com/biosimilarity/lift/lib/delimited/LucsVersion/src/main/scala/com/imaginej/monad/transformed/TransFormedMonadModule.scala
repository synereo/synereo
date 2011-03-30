package com.imaginej

package monad

package transformed

import monad.MonadModule

trait TransformedMonadModule
 extends MonadModule {
 type MonadModuleType <: MonadModule
 implicit val monadObject: MonadModuleType
 type FromC[+X] = monadObject.C[X]
 type FromIn[X] = monadObject.In[X]
 type FromOut[X] = monadObject.Out[X]
 type C[+X] <: TransformedMonadTrait[X]
 type ToC[+X]
 protected[monad] def _closeT[Y]: ToC[Y] => C[Y]
 def _lift[Y]: FromC[Y] => ToC[Y]
 override def _return[Y] =
  y =>
   _closeT {
    _lift {
     monadObject._return(y)
    }
   }
 trait TransformedMonadTrait[+X]
  extends Monad[X] { self: C[X] =>
  protected[monad] def openT: ToC[X]
 }
}
