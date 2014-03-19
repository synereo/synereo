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
  
  case class CC[+A,R]( k : ( A => R ) => R ) {
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
    ) extends LambdaExpr[V]

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

  object CPS {
    import LambdaCalculus._
    import MonadicEvidence._
    import CCMonad._
    import scala.collection.immutable.MapProxy
    import scala.collection.immutable.HashMap
    import scala.collection.immutable.Map

    type Target[V] = Box[V,AnyRef]    
    type Environment[V] = Map[V,Box[V,AnyRef]]
    
    trait CallByValue {
      def meaning[V](
        lambdaExpr : LambdaExpr[V]
      )( env : Environment[V] )( 
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]()
      ) : CC[Box[V,AnyRef],AnyRef] = {
        lambdaExpr match {
          case mention : Mention[V] =>
            innerMeaning[V]( mention )( env )( mc )
          case abstraction : Abstraction[V] =>
            innerMeaning[V]( abstraction )( env )( mc )
          case application : Application[V] =>
            innerMeaning[V]( application )( env )( mc )
          case v : Box[V,AnyRef] =>
            innerMeaning[V]( v )( env )( mc )
          case summation : Summation[V] =>
            innerMeaning[V]( summation )( env )( mc )
          case condition : Condition[V] =>
            innerMeaning[V]( condition )( env )( mc )
          case binding : Binding[V] =>
            innerMeaning[V]( binding )( env )( mc )
          case rbinding : RBinding[V] =>
            innerMeaning[V]( rbinding )( env )( mc )
          case shift : Shift[V] =>
            innerMeaning[V]( shift )( env )( mc )
          case reset : Reset[V] =>
            innerMeaning[V]( reset )( env )( mc )
        }        
      }

      def innerMeaning[V](
        m : Mention[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        env.get( m.v ) match {
          case Some( a ) => mc( a )
          case None => throw new Exception( "unbound variable: " + m.v )
        }
      }

      def innerMeaning[V](
        abs : Abstraction[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]()
      ) : CC[Box[V,AnyRef],AnyRef] = {
        mc(
          Value(
            ( v : Box[V,AnyRef] ) => {
              meaning[V]( abs.body )(
                ( env + ( abs.formal -> Value( v ) ) )
              )( mc )
            }
          )
        )
      }      
      
      def innerMeaning[V](
        app : Application[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        mc.bind(
          meaning[V]( app.operation )( env )( mc )
        )(
          ( boxK : Box[V,AnyRef] ) => {
            mc.bind(
              meaning[V]( app.actual )( env )( mc )
            )(
              ( v : Box[V,AnyRef] ) => {
                boxK match {
                  case Value( op : Function1[Box[V,AnyRef],CC[Box[V,AnyRef],AnyRef]] ) =>
                    op( v )
                  case _ => throw new Exception( "attempt to apply non-function: " + boxK )
                }
              }
            )
          }
        )
      }

      def innerMeaning[V](
         v : Box[V,AnyRef]
       )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
         mc( v )
       }

      def innerMeaning[V](
        sum : Summation[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        mc.bind(
          meaning[V]( sum.l )( env )( mc )
        )(
          ( v : Box[V,AnyRef] ) => {
            mc.bind(
              meaning[V]( sum.r )( env )( mc )
            )(
              ( w : Box[V,AnyRef] ) => {
                ( v, w ) match {
                  case ( Value( a ), Value( b ) ) => {
                    if ( ( a, b ).isInstanceOf[(Int,Int)] ) {
                      val s = a.asInstanceOf[Int] + b.asInstanceOf[Int]
                      mc( Value( s.asInstanceOf[AnyRef] ) )
                    }
                    else {
                      throw new Exception( "attempt to add non-numeric : " + a + ", " + b )
                    }                    
                  }
                  case _ =>
                    throw new Exception( "attempt to add non-values : " + v + ", " + w )
                }                
              }
            )
          }
        )
      }

      def innerMeaning[V](
        cond : Condition[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        mc.bind(
          meaning[V]( cond.test )( env )( mc )
        )(
          ( v : Box[V,AnyRef] ) => {
            v match {
              case Value( b ) => {
                if ( b.isInstanceOf[Boolean] ) {
                  if ( b.asInstanceOf[Boolean] ) {
                    meaning[V]( cond.tbranch )( env )( mc )
                  }
                  else {
                    meaning[V]( cond.fbranch )( env )( mc )
                  }
                }
                else {
                  throw new Exception( "attempting to test non-boolean : " + v )
                }
              }
              case _ => {
                throw new Exception( "attempting to test non-value : " + v )
              }
            }
          }
        )
      }      

      def innerMeaning[V](
        binding : Binding[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        mc.bind(
          meaning[V]( binding.actual )( env )( mc )
        )(
          ( v : Box[V,AnyRef] ) => {            
            val nenv : Environment[V] =
              env + ( binding.formal -> Value( v ) )
            meaning[V]( binding.body )( nenv )( mc )
          }
        )
      }

      def innerMeaning[V](
        binding : RBinding[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        lazy val k : Box[V,AnyRef] => CC[Box[V,AnyRef],AnyRef] =
          ( v : Box[V,AnyRef] ) => {    
            val nenv : Environment[V] =
              (
                ( env + ( binding.fnFormal -> Value( k ) ) )
                + ( binding.valueFormal -> Value( v ) )
              )
            meaning[V]( binding.actual )( nenv )( mc )
          }        
        val nenv =
          env + ( binding.fnFormal -> Value( k ) )
        meaning[V]( binding.body )( nenv )( mc )
      }

      def innerMeaning[V](
        shift : Shift[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {        

        val h : CC[Box[V,AnyRef],CC[AnyRef,AnyRef]] =
          CC(
            ( k : Box[V,AnyRef] => CC[AnyRef,AnyRef] ) => {
              meaning[V]( shift.body )(
                ( env + ( shift.fnFormal -> Value( k ) ) )
              )( mc ).asInstanceOf[CC[AnyRef,AnyRef]]
            }
          )
        mc.shift( h )
      }

      def innerMeaning[V](
        reset : Reset[V]
      )( env : Environment[V] )(
        mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef] = DCCMonad[AnyRef]() 
      ) : CC[Box[V,AnyRef],AnyRef] = {
        mc.reset(
          meaning[V]( reset.body )( env )( mc ).asInstanceOf[CC[AnyRef,AnyRef]]
        ).asInstanceOf[CC[Box[V,AnyRef],AnyRef]]
      }

      // implicit def cpsTranslator() : CallByValue = {
//         new CallByValue { }
//       }
    }
  }

  object CCExercise {
    import MonadicEvidence._
    import CCMonad._
    import LambdaCalculus._
    import CPS._

    implicit val translator = new CallByValue{ }

    // 1 + (reset (10 + (shift f . (f (f 100)))))
    // Expect 121
    def calculation1(
      env : Environment[String]
    )(
      implicit mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef]
    ) : CC[Box[String,AnyRef],AnyRef] = {
      val lexpr : LambdaExpr[String] =
        Summation[String](
          Value[String,AnyRef]( 1.asInstanceOf[AnyRef] ),
          Reset[String]( 
            Summation[String](
              Value[String,AnyRef]( 10.asInstanceOf[AnyRef] ),
              Shift[String]( 
                "f",
                Application[String](
                  Mention[String]( "f" ),
                  Application[String](
                    Mention[String]( "f" ),
                    Value[String,AnyRef]( 100.asInstanceOf[AnyRef] )
                  )
                )
              )
            )
          )
        )

      translator.meaning[String]( lexpr )( env )( mc )
    }

    def calculation2(
      env : Environment[String]
    )(
      implicit mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef]
    ) : CC[Box[String,AnyRef],AnyRef] = {
      val lexpr =
        Summation[String](
          Value[String,AnyRef]( 1.asInstanceOf[AnyRef] ),
          Reset[String]( 
            Summation[String](
              Value[String,AnyRef]( 10.asInstanceOf[AnyRef] ),
              Shift[String]( 
                "f",
                Value[String,AnyRef]( 100.asInstanceOf[AnyRef] )
              )
            )
          )
        )

      translator.meaning( lexpr )( env )( mc )
    }

    def calculation3(
      env : Environment[String]
    )(
      implicit mc : Monad[({type L[A] = CC[A,AnyRef]})#L] with DelimitedCC[AnyRef]
    ) : CC[Box[String,AnyRef],AnyRef] = {
      val lexpr =
        Summation[String](
          Value[String,AnyRef]( 1.asInstanceOf[AnyRef] ),
          Reset[String]( 
            Summation[String](
              Value[String,AnyRef]( 10.asInstanceOf[AnyRef] ),
              Shift[String]( 
                "f",
                Summation[String](
                  Application[String](
                    Mention[String]( "f" ),
                    Value[String,AnyRef]( 100.asInstanceOf[AnyRef] )
                  ),
                  Application[String](
                    Mention[String]( "f" ),
                    Value[String,AnyRef]( 1000.asInstanceOf[AnyRef] )
                  )
                )
              )
            )
          )
        )

      translator.meaning( lexpr )( env )( mc )
    }
  }
}
