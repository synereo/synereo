package com.imaginej

package monad

package control.transformed

import com.imaginej.monad.control.ControlMonadModule
import com.imaginej.monad.transformed.TransformedMonadModule

trait ControlTransformedMonadModule
 extends TransformedMonadModule
 with ControlMonadModule {
 type C[+X] <: ControlTransformedMonadTrait[X]
 type ToC[+X] = FromC[CCV[B, C, X]]
 type In[-X] = Unit
 override def _lift[Y] =
  cy =>
   for { y <- cy } yield Iru(y)
 override def _withSubCont[W]: Prompt[B, C, W] => MC[B, C, A, W] => C[A] =
  p => mc =>
  _closeT {
   monadObject._return {
    Deru((ca: C[A]) => ca, p._1(mc))
   }
  }
 // -F-
 def _min_f_min[W]: Prompt[B, C, W] => MC[B, C, A, W] => C[A] =
  _withSubCont[W]
 // -F+
 def min_f_plus[W]: Prompt[B, C, W] => ((A => C[W]) => C[W]) => C[A] =
  p => mc =>
   _min_f_min(p) { sc =>
    mc(_return(_).pushSubCont(sc).pushPrompt(p))
  }
 // +F+
 def plus_f_plus[W]: Prompt[B, C, W] => ((A => C[W]) => C[W]) => C[A] =
  p => mc =>
   _min_f_min(p) { sc =>
    mc(_return(_).pushSubCont(sc).pushPrompt(p)).pushPrompt(p)
  } 
 // +F-
 def plus_f_min[W]: Prompt[B, C, W] => ((A => C[W]) => C[W]) => C[A] =
  p => mc =>
   _min_f_min(p) { sc =>
    mc(_return(_).pushSubCont(sc)).pushPrompt(p)
  } 
 trait ControlTransformedMonadTrait[+X]
  extends TransformedMonadTrait[X]
  with ControlMonad[X] { self: C[X] =>
  def >=-[Y](xf_cy: X => C[Y]) = {
   _closeT {
    this.openT flatMap {
     case Iru(x) =>
      xf_cy(x).openT
     case Deru(sc, b) =>
      monadObject._return(Deru(sc((_: C[A])) flatMap xf_cy, b))
    }
   }
  }
  override def pushSubCont[V >: X, Y, W >: Y](sc: SC[B, C, V, W]): C[W] =
   sc(this)
  override def pushPrompt[W >: X](p: Prompt[B, C, W]): C[W] = {
   _closeT {
    this.openT flatMap {
     case e@Iru(_) =>
      monadObject._return(e)
     case Deru(sc, b) =>
      p._2(b) match {
       case Some(mc) =>
        mc(sc).openT
       case None =>
        monadObject._return(Deru(sc(_: C[A]).pushPrompt(p), b))
     }
    }
   }
  }
  def abort[W >: X](p: Prompt[B, C, W]): C[A] =
   _withSubCont[W](p)(_ => this)
  def runCC[W >: X](u: Unit)(i: FromIn[W]): FromOut[W] = {
   this.openT flatMap {
    case Iru(w) => monadObject._return(w)
    case _ => throw new Exception("Escaping bubble: you have forgotten pushPrompt")
   } run (i)
  }  
 }
}
