// -*- mode: Scala;-*- 
// Filename:    K.scala 
// Authors:     lgm                                                    
// Creation:    Sun Mar  9 15:12:13 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.monad

object CCMonad {
  import MonadicEvidence._

  // This version is from Wadler's Composable Continuations paper.
  
  case class CC[A,R]( k : ( A => R ) => R ) {
    def apply( f : A => R ) : R = k( f )
  }  

  implicit def CCFunctor[R]() : Functor[({type L[A] = CC[A,R]})#L] =
    new Functor[({type L[A] = CC[A,R]})#L] {
      def fmap[V, P >: V, U]( f : P => U ) : CC[P,R] => CC[U,R] = {
	( k : CC[P,R] ) => {
	  new CC[U,R](
	    ( nk : U => R ) => {
              k( ( p : P ) => nk( f( p ) ) )
	    }
	  )
	}
      }
    }

  implicit def CCMonad[R]() : Monad[({type L[A] = CC[A,R]})#L] =
    new Monad[({type L[A] = CC[A,R]})#L] {      
      def apply[A]( data : A ) = new CC(( k : A => R ) => k( data ) )      
      def flatten[A]( m : CC[CC[A,R],R] ) : CC[A,R] =
        new CC[A,R](
	  ( k : A => R ) => {
	    m( ( kk : CC[A,R] ) => kk( k ) )
	  }
        )
    }

  trait DelimitedCC[R] {
    def shift[A]( h : CC[A,CC[R,R]] ) : CC[A,R] = {
      new CC[A,R](
        ( k : A => R ) => {
          h(
            ( a : A ) => {
              new CC[R,R]( ( r : R => R ) => r( k( a ) ) )
            }
          )( ( r : R ) => r )            
        }
      )
    }

    def reset( k : CC[R,R] ) : CC[R,R] = {
      new CC[R,R](
        ( nk : R => R ) => {
          k( ( r : R ) => r )
        }
      )
    }
  }

  implicit def DCCMonad[R]() : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] =
    new Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] {      
      val kmonad = CCMonad[R]()
      def apply[A]( data : A ) = kmonad( data )
      def flatten[A]( m : CC[CC[A,R],R] ) : CC[A,R] = kmonad.flatten( m )
    }
}

object PCMonad {
  import MonadicEvidence._
  import ParametricMonadicEvidence._

  // This version is from Atkey's Parametric Monads paper.

  case class Continuation[+A,-B,+C]( val k : ( A => B ) => C ) {
    def apply( f : A => B ) : C = k( f )
    def map( f : A => B ) : C = this( f )
  }

  implicit def pcMonad[A,B,C]( c : Continuation[A,B,C] ) : PMonad[Continuation] = {
    new PMonad[Continuation] {
      def apply[S,T]( s : S ) : Continuation[S,T,T] =
        Continuation[S,T,T]( ( k : S => T ) => k( s ) )
      def flatten[S,T,U,V] (      
        kks : Continuation[Continuation[S,V,U],U,T]
      ) : Continuation[S,V,T] = {      
        Continuation[S,V,T]( 
          ( s2v : S => V ) => {
	    kks.map(
	      ( csvu : Continuation[S,V,U] ) => {
	        csvu.map( s2v )
	      }
	    )
          }
        )
      }
      def strength [S1,S2,T,U](
        s1 : S1, cs2tu : Continuation[S2,T,U] 
      ) : Continuation[Tuple2[S1,S2],T,U] = {
        Continuation[Tuple2[S1,S2],T,U] (
          ( s1s22t : (Tuple2[S1,S2] => T) ) => {
	    cs2tu.map(
	      ( s2 : S2 ) => {
	        val s1s2 : Tuple2[S1,S2] = ( s1, s2 )
	          s1s22t( s1s2 )
	      }
	    )
          }
        )
      }
    }
  }

  trait DelimitedContinuation[X,Y,Z] {
    self : PMonad[Continuation] =>
    def reset [A,B,C] (
      c : Continuation[A,A,B]
    ) : Continuation[B,C,C] = {
      Continuation[B,C,C](
        ( b2c2c : ( B => C ) ) => {
	  b2c2c( c.map( ( a : A ) => a ) )
        }
      )
    }
    def shift [A,B,C,D,E] (
      a2cbcc2cdde : ( A => Continuation[B,C,C] ) => Continuation[D,D,E]
    ) : Continuation[A,B,E] = {
      Continuation[A,B,E](
        ( a2b : A => B ) => {
	  a2cbcc2cdde(
	    ( a : A ) => {
	      apply[B,C]( a2b( a ) )
	    }
	  ).map( ( d : D ) => d )
        }
      )
    }
  }
}

package usage {
  object LambdaCalculus {
    import MonadicEvidence._
    trait LambdaExpr[+V] {
      def +[V1 >: V]( lexpr : LambdaExpr[V1] ) : LambdaExpr[V1] = 
        Summation[V1]( this, lexpr )
    }
    case class Mention[+V]( v : V ) extends LambdaExpr[V]
    case class Abstraction[+V](
      formal : V,
      body : LambdaExpr[V]
    ) extends LambdaExpr[V]
    case class Application[+V](
      operation : LambdaExpr[V],
      actual : LambdaExpr[V]
    )

    trait Box[+V,+S] { def s : S }
    case class Value[+V,+S]( s : S ) extends LambdaExpr[V] with Box[V,S]
    case object Empty extends LambdaExpr[Nothing] with Box[Nothing,Nothing] {
      override def s : Nothing = throw new Exception( "Can't get something from Nothing" )
    }

    case class Summation[+V](
      l : LambdaExpr[V],
      r : LambdaExpr[V]
    ) extends LambdaExpr[V]
    case class Condition[+V](
      test : LambdaExpr[V],
      tbranch : LambdaExpr[V],
      fbranch : LambdaExpr[V]
    ) extends LambdaExpr[V]
    case class Binding[+V](
      formal : V,
      actual : LambdaExpr[V],
      body : LambdaExpr[V]
    ) extends LambdaExpr[V]
    case class RBinding[+V](
      fnFormal : V,
      valueFormal : V,
      actual : Abstraction[V],
      body : LambdaExpr[V]
    ) extends LambdaExpr[V]
    case class Shift[+V](
      fnFormal : V,
      body : LambdaExpr[V]
    ) extends LambdaExpr[V]
    case class Reset[+V](
      body : LambdaExpr[V]
    ) extends LambdaExpr[V]

    implicit def boxMonad[V]() = new Monad[({type L[S] = Box[V,S]})#L] {      
      def apply[S]( data : S ) = Value[V,S]( data )
      def flatten[S]( m : Box[V,Box[V,S]] ) : Box[V,S] =
        m match {
          case Value( Value( s ) ) => Value( s )
          case _ => Empty
        }
    }
  }

//   object CPS {
//     import LambdaCalculus._
//     import MonadicEvidence._
//     import CCMonad._
//     import scala.collection.immutable.MapProxy
//     import scala.collection.immutable.HashMap
//     import scala.collection.immutable.Map

//     type Target[V] = Box[V,AnyRef]    
//     type Environment[V] = Map[V,Box[V,AnyRef]]
    
//     trait CallByValue {
//       def meaning[V](
//         lambdaExpr : LambdaExpr[V]
//       )( env : Environment[V] )( mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() ) : Box[V,AnyRef] = {
//         lambdaExpr match {
//           case mention : Mention[V] =>
//             innerMeaning[V]( mention )( env )( mc )
//           case abstraction : Abstraction[V] =>
//             innerMeaning[V]( abstraction )( env )( mc )
//           case application : Application[V] =>
//             innerMeaning[V]( application )( env )( mc )
//         }        
//       }

//       def innerMeaning[V](
//         m : Mention[V]
//       )( env : Environment[V] )( mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() ) : Box[V,AnyRef] = {
//         env.get( m.v ) match {
//           case Some( a ) => Value( mc( a ) )
//           case None => throw new Exception( "unbound variable: " + m.v )
//         }
//       }

//       def innerMeaning[V](
//         abs : Abstraction[V]
//       )( env : Environment[V] )( mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() ) : Box[V,AnyRef] = {
//         Value(
//           mc(
//             ( v : Box[V,AnyRef] => Box[V,AnyRef] ) => {
//               meaning[V]( abs.body )(
//                 ( env + ( abs.formal -> Value( v ) ) )
//               )( mc )
//             }
//           )
//         )
//       }      
      
//       def innerMeaning[V](
//         app : Application[V]
//       )( env : Environment[V] )( mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() ) : Box[V,AnyRef] = {
//         Value(
//           mc.bind[Box[V,AnyRef],Box[V,AnyRef],Box[V,AnyRef]](
//             mc( meaning[V]( app.operation )( env )( mc ) )
//           )(
//             ( boxK : Box[V,AnyRef] ) => {
//               mc.bind[Box[V,AnyRef],Box[V,AnyRef],Box[V,AnyRef]](
//                 mc( meaning[V]( app.actual )( env )( mc ) )
//               )(
//                 ( v : Box[V,AnyRef] ) => {
//                   for( k <- monadToComprehension( boxK ) ) yield {
//                     k match {
//                       case op : Function1[_,_] =>
//                         op( v )
//                       case _ => throw new Exception( "attempt to apply non-function: " + k )
//                     }
//                   }
//                 }
//               )
//             }
//           )
//         )
//       }

//       implicit def cpsTranslator() : CallByValue = {
//         new CallByValue { }
//       }
//     }
//   }
// object CPS {
//     import LambdaCalculus._
//     import MonadicEvidence._
//     import CCMonad._
//     import scala.collection.immutable.MapProxy
//     import scala.collection.immutable.HashMap
//     import scala.collection.immutable.Map

//     type Target[V,R,S] = CC[Box[V,S],R]    
//     type Environment[V,S] = Map[V,Box[V,S]]
    
// //     trait CallByValue {
// //       // This is where things get interesting!
// //       // If this is a translator, then we require that the
// //       // syntax of CPS be monadic in V. If this is a compiler, then we
// //       // require that the target interpretation of CPS be monadic
// //       // in V. 
// //       def meaning[V,R,S](
// //         lambdaExpr : LambdaExpr[V]
// //       )( env : Environment[V] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         lambdaExpr match {
// //           case mention : Mention[V] =>
// //             innerMeaning[V,R,S]( mention )( env )( mc )
// //           case abstraction : Abstraction[V] =>
// //             innerMeaning[V,R,S]( abstraction )( env )( mc )
// //           case application : Application[V] =>
// //             innerMeaning[V,R,S]( application )( env )( mc )
// //           case v : Value[V] =>
// //             innerMeaning[V,R,S]( v )( env )( mc )
// //           case summation : Summation[V] =>
// //             innerMeaning[V,R,S]( summation )( env )( mc )
// //           case condition : Condition[V] =>
// //             innerMeaning[V,R,S]( condition )( env )( mc )
// //           case binding : Binding[V] =>
// //             innerMeaning[V,R,S]( binding )( env )( mc )
// //           case rbinding : RBinding[V] =>
// //             innerMeaning[V,R,S]( rbinding )( env )( mc )
// //           case shift : Shift[V] =>
// //             innerMeaning[V,R,S]( shift )( env )( mc )
// //           case reset : Reset[V] =>
// //             innerMeaning[V,R,S]( reset )( env )( mc )
// //         }        
// //       }

// //       def innerMeaning[V,R,S](
// //         m : Mention[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         env.get( m.v ) match {
// //           case Some( a ) => mc( a )          
// //           case None => throw new Exception( "unbound variable: " + m.v )
// //         }
// //       }

// //       def innerMeaning[V,R,S](
// //         abs : Abstraction[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc(
// //           ( v : Box[V,S] => Box[V,S] ) => {
// //             meaning[V,R,S]( abs.body )(
// //               ( env + ( abs.formal -> v ) )
// //             )( mc )
// //           }
// //         )
// //       }

// //       def innerMeaning[V,R,S](
// //         app : Application[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc.bind(
// //           meaning[V,R,S]( app.operation )( env )( mc ) 
// //         )(
// //           ( k ) => {
// //             val mAct = mc( meaning[V,R,S]( app.actual )( env )( mc ) )
// //             val mApp = mc.bind( mAct )( ( v ) => mAct( v ) )                
// //             mApp( k )
// //           }
// //         )
// //       }
      
// //       def innerMeaning[V,R,S](
// //         v : Value[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc( v.n )
// //       }

// //       def innerMeaning[V,R,S](
// //         sum : Summation[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc.bind(
// //           meaning[V,R,S]( sum.l )( env )( mc )
// //         )(
// //           ( v ) => {
// //             mc.bind(
// //               meaning[V,R,S]( sum.r )( env )( mc )
// //             )(
// //               ( w ) => {
// //                 mc( v + w )
// //               }
// //             )
// //           }
// //         )
// //       }

// //       def innerMeaning[V,R,S](
// //         cond : Condition[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc.bind(
// //           meaning[V,R,S]( cond.test )( env )( mc )
// //         )(
// //           ( v ) => {
// //             if ( v ) {
// //               meaning[V,R,S]( cond.tbranch )( env )( mc )
// //             }
// //             else {
// //               meaning[V,R,S]( cond.fbranch )( env )( mc )
// //             }
// //           }
// //         )
// //       }

// //       def innerMeaning[V,R,S](
// //         binding : Binding[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc.bind(
// //           meaning[V,R,S]( binding.actual )( env )( mc )
// //         )(
// //           ( v ) => {            
// //             val nenv : Environment[V,S] =
// //               env + ( binding.formal -> v )
// //             meaning[V,R,S]( binding.body )( nenv )( mc )
// //           }
// //         )
// //       }

// //       def innerMeaning[V,R,S](
// //         binding : RBinding[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         lazy val k =
// //           ( v ) => {    
// //             val nenv : Environment[V,S] =
// //               (
// //                 ( env + ( binding.fnFormal -> k ) )
// //                 + ( binding.valueFormal -> v )
// //               )
// //             meaning[V,R,S]( binding.actual )( nenv )( mc )
// //           }        
// //         val nenv =
// //           env + ( binding.fnFormal -> k )
// //         meaning[V,R,S]( binding.body )( nenv )( mc )
// //       }

// //       def innerMeaning[V,R,S](
// //         shift : Shift[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc.shift(
// //           ( k ) => {
// //             val nenv : Environment[V,S] =
// //               env + ( shift.fnFormal -> k )
// //             meaning[V,R,S]( shift.body )( nenv )( mc )
// //           }
// //         )
// //       }

// //       def innerMeaning[V,R,S](
// //         reset : Reset[V]
// //       )( env : Environment[V,S] )( mc : Monad[({type L[A] = CC[A,R]})#L] with DelimitedCC[R] = DCCMonad[R]() ) : Target[V,R,S] = {
// //         mc.reset(
// //           meaning[V,R,S]( reset.body )( env )( mc )
// //         )
// //       }
// //     }

// //     implicit def cpsTranslator() : CallByValue = {
// //       new CallByValue { }
// //     }
// //   }

//   object CCExercise {
//     import MonadicEvidence._
//     import CCMonad._
//     import LambdaCalculus._
//     import CPS._

//     implicit val translator = new CallByValue{ }

//     // 1 + (reset (10 + (shift f . (f (f 100)))))
//     // Expect 121
//     def calculation1(
//       implicit witness : Monad[({type L[A] = CC[A,Int]})#L] with DelimitedCC[Int]
//     ) : AnyRef = {
//       val lexpr =
//         Summation(
//           Value( 1 ),
//           Reset( 
//             Summation(
//               Value( Shift ),
//               10 + ( 
//                 "f",
//                 Application(
//                   Mention( "f" ),
//                   Application(
//                     Mention( "f" ),
//                     Value( 100 )
//                   )
//                 )
//               )
//             )
//           )
//         )

//       translator.meaning( lexpr )
//     }

//     def calculation2(
//       implicit witness : Monad[({type L[A] = CC[A,Int]})#L] with DelimitedCC[Int]
//     ) : AnyRef = {
//       val lexpr =
//         Summation(
//           Value( 1 ),
//           Reset( 
//             Summation(
//               Value( 10 ),
//               Shift( 
//                 "f",
//                 Value( 100 )
//               )
//             )
//           )
//         )

//       translator.meaning( lexpr )
//     }

//     def calculation3(
//       implicit witness : Monad[({type L[A] = CC[A,Int]})#L] with DelimitedCC[Int]
//     ) : AnyRef = {
//       val lexpr =
//         Summation(
//           Value( 1 ),
//           Reset( 
//             Summation(
//               Value( 10 ),
//               Shift( 
//                 "f",
//                 Summation(
//                   Application(
//                     Mention( "f" ),
//                     Value( 100 )
//                   ),
//                   Application(
//                     Mention( "f" ),
//                     Value( 1000 )
//                   )
//                 )
//               )
//             )
//           )
//         )

//       translator.meaning( lexpr )
//     }
//   }
}
