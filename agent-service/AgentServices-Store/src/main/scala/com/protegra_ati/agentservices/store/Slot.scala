// -*- mode: Scala;-*- 
// Filename:    Slot.scala 
// Authors:     lgm                                                    
// Creation:    Mon Jul 21 13:19:18 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.trampoline
package mongodb
package record
package slot

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

object Slot extends Serializable {
  import com.biosimilarity.lift.trampoline.mongodb.record.EvalHandlerService
  import com.biosimilarity.evaluator.distribution.DSLCommLink.mTT
  import com.biosimilarity.evaluator.distribution.ConcreteHL._

  def onCompletion[B](
    fail : String => Unit,
    succeed : String => B,
    unpack : Boolean = false //true
  )( s : String ) : Option[EvalHandlerService.Rsrc] => Unit = {
    if ( unpack ) {
      ( optRsrc : Option[EvalHandlerService.Rsrc] ) => {
        optRsrc match {
          case None => () ;
          case Some( mTT.Ground( Bottom ) ) => {
            fail( s + ": Undefined" );
            ()
          }
          case Some( mTT.Ground(PostedExpr( ( Bottom, _, _, _ ) ) ) ) => {
            fail( s + ": Undefined" );
            ()
          }
          case Some( mTT.Ground(PostedExpr( ( v : String, _, _, _ ) ) ) ) => {
            succeed( v );
            ()
          }
          case Some( mTT.Ground(PostedExpr( ( PostedExpr( v : String ), _, _, _ ) ) ) ) => {
            succeed( v );
            ()
          }
          case Some( mTT.RBoundHM( Some( mTT.Ground( v ) ), _ ) ) => {
            v match {
              case Bottom => { fail( s + ": Undefined" ); () }
              case PostedExpr( ( PostedExpr( s : String ), _, _, _ ) ) => {
                succeed( s );
                ()
              }
              case _ => {
                fail( s + ": Unrecognized (bound) resource: " + optRsrc );
                ()
              }
            }
          }
          case _ => {
            fail( s + ": Unrecognized resource: " + optRsrc )
            ()
          }
        }
      }
    }
    else {
      ( optRsrc : Option[EvalHandlerService.Rsrc] ) => { 
        succeed( optRsrc.toString );
        ()
      }
    }
  }

  trait SlotT[M[_], A] {
    def feed(): MonadT[M, A]
    def fetch(): MonadT[M, A]
    def get(): MonadT[M, A]
    def read(): MonadT[M, A]
    //def subscribe(): MonadT[M, A]
    
    def put( value : A ) : MonadT[M, Unit]
    def post[V <: java.io.Serializable]( value : V ) : MonadT[M, Unit]
  }

  type U[A] = Unit

  case class DieselSlot(
    val pac: PortableAgentCnxn,
    val label: CnxnCtxtLabel[String,String,String],
    val fail: String => Unit // Failure continuations receive a reason.
  ) extends SlotT[U, String] {        
    case class Fetcher() extends MonadT[U, String] {
      private val fetcher = this
      def map[B](succeed: String => B): Unit = {
        EvalHandlerService.fetch(
          label,
          List(pac),
          onCompletion[B]( fail, succeed )( "fetch" )
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      def withFilter(filter: String => Boolean): MonadT[U, String] =
        new MonadT[U, String] with Serializable {
          def map[B](succeed: String => B): Unit = fetcher.map((str: String) => {
            if (filter(str)) { succeed(str) }
            ()
          })
          def unit[B](b: B): Unit = ()
            def flatten[B](mmb: Unit): Unit = ()
              def withFilter(filter: String => Boolean): MonadT[U, String] 
          = fetcher.withFilter(filter)
        }
    }

    case class Reader() extends MonadT[U, String] {
      private val reader = this
      def map[B](succeed: String => B): Unit = {
        EvalHandlerService.read(
          label,
          List(pac),
          onCompletion[B]( fail, succeed )( "read" )
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      def withFilter(filter: String => Boolean): MonadT[U, String] =
        new MonadT[U, String] with Serializable {
          def map[B](succeed: String => B): Unit = reader.map((str: String) => {
            if (filter(str)) { succeed(str) }
            ()
          })
          def unit[B](b: B): Unit = ();
          def flatten[B](mmb: Unit): Unit = ();
          def withFilter(filter: String => Boolean): MonadT[U, String] =
            reader.withFilter(filter)
        }
    }

    case class Feeder() extends MonadT[U, String] {
      private val feeder = this
      def eMap[B](succeed: String => B): Unit = {
        println( EvalHandlerService )
        EvalHandlerService.feed(
          label,
          List(pac),
          ( optRsrc ) => println( "received: " + optRsrc )
        )
      }
      def map[B](succeed: String => B): Unit = {
        EvalHandlerService.feed(
          label,
          List(pac),
          onCompletion[B]( fail, succeed )( "feed" )
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      def withFilter(filter: String => Boolean): MonadT[U, String] = new MonadT[U, String] {
        def map[B](succeed: String => B): Unit = feeder.map((str: String) => {
          if (filter(str)) { succeed(str) }
          ()
        })
        def unit[B](b: B): Unit = ()
        def flatten[B](mmb: Unit): Unit = ()
        def withFilter(filter: String => Boolean): MonadT[U, String] 
          = feeder.withFilter(filter)
      }
    }

    case class Getter() extends MonadT[U, String] {
      private val getter = this
      def map[B](succeed: String => B): Unit = {
        EvalHandlerService.get(
          label,
          List(pac),
          onCompletion[B]( fail, succeed )( "get" )
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      def withFilter(filter: String => Boolean): MonadT[U, String] =
        new MonadT[U, String] with Serializable {
          def map[B](succeed: String => B): Unit = getter.map((str: String) => {
            if (filter(str)) { succeed(str) }
            ()
          })
          def unit[B](b: B): Unit = ();
          def flatten[B](mmb: Unit): Unit = ();
          def withFilter(filter: String => Boolean): MonadT[U, String] =
            getter.withFilter(filter)
      }
    }

    // case class Subscription() extends MonadT[U, String] {
//       private val subscription = this
//       def map[B](succeed: String => B): Unit = {
//         EvalHandlerService.subscribe(
//           label,
//           List(pac),
//           onCompletion[B]( fail, succeed )( "subscription" )
//         )
//       }
//       def unit[B](b: B): Unit = ()
//       def flatten[B](mmb: Unit): Unit = ()
//       def withFilter(filter: String => Boolean): MonadT[U, String] =
//         new MonadT[U, String] with Serializable {
//           def map[B](succeed: String => B): Unit = subscription.map((str: String) => {
//             if (filter(str)) { succeed(str) }
//             ()
//           })
//           def unit[B](b: B): Unit = ()
//             def flatten[B](mmb: Unit): Unit = ()
//               def withFilter(filter: String => Boolean): MonadT[U, String] 
//           = subscription.withFilter(filter)
//         }
//     }

    case class Putter(value: String) extends MonadT[U, Unit] {
      private val putter = this
      def map[B](succeed: Unit => B): Unit = {
        EvalHandlerService.put(
          label,
          List(pac),
          value,
          (optRsrc) => {
            optRsrc match {
              case None => () ;
              case Some( mTT.Ground( PostedExpr( ( Bottom, _, _, _ ) ) ) ) => succeed()
              case Some(mTT.RBoundHM(Some(mTT.Ground( _ )), _)) => succeed()
              case _ => fail("put: Unrecognized resource: " + optRsrc)
            }
          }
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      
      def withFilter(filter: Unit => Boolean): MonadT[U, Unit] =
        new MonadT[U, Unit] with Serializable {
          def map[B](succeed: Unit => B): Unit = putter.flatMap((u: Unit) => {
            if (filter()) { succeed() }
            ()
          })
          def unit[B](b: B): Unit = ();
          def flatten[B](mmb: Unit): Unit = ();
          def withFilter(filter: Unit => Boolean): MonadT[U, Unit] = 
            putter.withFilter(filter)
        }
    }

    case class Poster[Value](value: Value) extends MonadT[U, Unit] {
      private val poster = this
      def map[B](succeed: Unit => B): Unit = {
        EvalHandlerService.post[Value](
          label,
          List(pac),
          value,
          (optRsrc) => {
            optRsrc match {
              case None => () ;
              case Some( mTT.Ground( PostedExpr( ( Bottom, _, _, _ ) ) ) ) => succeed()
              case Some(mTT.RBoundHM(Some(mTT.Ground( _ )), _)) => succeed()
              case _ => fail("post: Unrecognized resource: " + optRsrc)
            }
          }
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      
      def withFilter(filter: Unit => Boolean): MonadT[U, Unit] =
        new MonadT[U, Unit] with Serializable {
          def map[B](succeed: Unit => B): Unit = poster.flatMap((u: Unit) => {
            if (filter()) { succeed() }
            ()
          })
          def unit[B](b: B): Unit = ();
          def flatten[B](mmb: Unit): Unit = ();
          def withFilter(filter: Unit => Boolean): MonadT[U, Unit] = 
            poster.withFilter(filter)
      }
    }

    // consumption
    def feed() = Feeder()
    def fetch() = Fetcher()        
    def get() = Getter()
    def read() = Reader()
//    def subscribe() = Subscription()
    // production
    def put( value : String ) = Putter( value )
    def post[V <: java.io.Serializable]( value : V ) = Poster( value )
  }

  case class LoggingForwarderSlot[M[_]](
    target: SlotT[M, String],
    onFetch: String => Unit,
    onFeed: String => Unit,
    onGet: String => Unit,
    onRead: String => Unit,
//    onSubscribe: String => Unit,
    onPut: String => Unit,
    onPost: String => Unit
  ) extends SlotT[M, String] {
    def feed() = new MonadT[M, String] with Serializable {
      val t = target.feed()
      def map[B](f: String => B): M[B] = t.map((a: String) => { onFeed(a); f(a) })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
    def fetch() = new MonadT[M, String] with Serializable {
      val t = target.fetch()
      def map[B](f: String => B): M[B] = t.map((a: String) => { onFetch(a); f(a) })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
    def get() = new MonadT[M, String] with Serializable {
      val t = target.get()
      def map[B](f: String => B): M[B] = t.map((a: String) => { onGet(a); f(a) })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
    def read() = new MonadT[M, String] with Serializable {
      val t = target.read()
      def map[B](f: String => B): M[B] = t.map((a: String) => { onRead(a); f(a) })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
//     def subscribe() = new MonadT[M, String] with Serializable {
//       val t = target.subscribe()
//       def map[B](f: String => B): M[B] = t.map((a: String) => { onSubscribe(a); f(a) })
//       def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
//       def unit[B](b: B): M[B] = t.unit(b)
//     }
    def put(value: String) = new MonadT[M, Unit] with Serializable {
      val t = target.put(value)
      def map[B](f: Unit => B): M[B] = t.map((x: Unit) => { onPut(value); f() })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
    def post[V <: java.io.Serializable]( value : V ) =
      new MonadT[M, Unit] with Serializable {
        val t = target.post[V]( value )
        def map[B](f: Unit => B): M[B] = t.map((x: Unit) => { onPost(value + "" ); f() })
        def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
        def unit[B](b: B): M[B] = t.unit(b)
      }
  }
  
  case class SerializingSlot[M[_], A <: java.io.Serializable](target: SlotT[M, String]) extends SlotT[M, A] {
    import biz.source_code.base64Coder.Base64Coder
    import java.io._
    
    def feed() = new MonadT[M, A] with Serializable {
      val t = target.feed()
      def deserialize(s: String): A = {
        val data : Array[Byte] = Base64Coder.decode(s)
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[A]
      }
      def map[B](f: A => B): M[B] = t.map(f compose deserialize)
      def unit[B](b: B) = t.unit(b)
      def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
    }
    def fetch() = new MonadT[M, A] with Serializable {
      val t = target.fetch()
      def deserialize(s: String): A = {
        val data : Array[Byte] = Base64Coder.decode(s)
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[A]
      }
      def map[B](f: A => B): M[B] = t.map(f compose deserialize)
      def unit[B](b: B) = t.unit(b)
      def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
    }
    def get() = new MonadT[M, A] with Serializable {
      val t = target.get()
      def deserialize(s: String): A = {
        val data : Array[Byte] = Base64Coder.decode(s)
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[A]
      }
      def map[B](f: A => B): M[B] = t.map(f compose deserialize)
      def unit[B](b: B) = t.unit(b)
      def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
    }
    def read() = new MonadT[M, A] with Serializable {
      val t = target.read()
      def deserialize(s: String): A = {
        val data : Array[Byte] = Base64Coder.decode(s)
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[A]
      }
      def map[B](f: A => B): M[B] = t.map(f compose deserialize)
      def unit[B](b: B) = t.unit(b)
      def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
    }
//     def subscribe() = new MonadT[M, A] with Serializable {
//       val t = target.subscribe()
//       def deserialize(s: String): A = {
//         val data : Array[Byte] = Base64Coder.decode(s)
//         val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
//         val o : java.lang.Object = ois.readObject()
//         ois.close()
//         o.asInstanceOf[A]
//       }
//       def map[B](f: A => B): M[B] = t.map(f compose deserialize)
//       def unit[B](b: B) = t.unit(b)
//       def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
//     }
    
    def put(value: A) = new MonadT[M, Unit] with Serializable {
      val t = target.put(serialize(value))
      def serialize[T <: java.io.Serializable](t: T): String = {
        val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos : ObjectOutputStream = new ObjectOutputStream( baos )
        oos.writeObject( t.asInstanceOf[java.io.Serializable] )
        oos.close()
        new String( Base64Coder.encode( baos.toByteArray() ) )
      }
      def map[B](f: Unit => B): M[B] = t.map(f)
      def unit[B](b: B) = t.unit(b)
      def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
    }
    def post[V <: java.io.Serializable]( value : V ) =
      new MonadT[M, Unit] with Serializable {
        val t = target.post[String](serialize(value))
        def serialize[T >: V](t: T): String = {
          val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
          val oos : ObjectOutputStream = new ObjectOutputStream( baos )
          oos.writeObject( t.asInstanceOf[java.io.Serializable] )
          oos.close()
          new String( Base64Coder.encode( baos.toByteArray() ) )
        }
        def map[B](f: Unit => B): M[B] = t.map(f)
        def unit[B](b: B) = t.unit(b)
        def flatten[B](mmb: M[M[B]]) = t.flatten(mmb)
      }
  }
}














