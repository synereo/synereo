package com.imaginej

package objects

object UsingMonads {
 def usingIdentityMonad() {
  import com.imaginej.monad.instances.IdentityMonadObject._
  println {
   { 
     for {
      hello <- _return("Hello")
      comma <- _return(',')
      world <- _return(" World")
      bang <- _return('!')
     } yield hello+comma+world+bang
   } run(())
  }
 }
 def main(args: Array[String]) {
  usingIdentityMonad()
 }
}
