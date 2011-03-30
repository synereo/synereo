package com.imaginej

package objects

object UsingStateMonads {
 def usingStateTransformedIdentityMonad() {
  import monad.state.instances.IntStateTransformedIdentityMonadObject._
  println {
   { 
     for {
      s <- _get(())
     } yield s
   } run (0)
  }
  println {
   _changeAndTransform (_ + 1) (_ + 1) run (0)
  }
 }
 def main(args: Array[String]) {
  usingStateTransformedIdentityMonad()
 }
}
