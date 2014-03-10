package com.biosimilarity.evaluator.spray.util

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

object Slot extends Serializable {
  trait SlotT[M[_], A] {
    def fetch(): MonadT[M, A]
    def put(value: A): MonadT[M, Unit]
  }

  type U[A] = Unit

  case class DieselSlot(
    val pac: PortableAgentCnxn,
    val label: CnxnCtxtLabel[String,String,String],
    val fail: String => Unit // Failure continuations receive a reason.
  ) extends SlotT[U, String] {
    import com.biosimilarity.evaluator.spray.EvalHandlerService
    import com.biosimilarity.evaluator.distribution.DSLCommLink.mTT
    import com.biosimilarity.evaluator.distribution.ConcreteHL._
    
    case class Fetcher() extends MonadT[U, String] {
      private val fetcher = this
      def map[B](succeed: String => B): Unit = {
        EvalHandlerService.agentMgr().fetch(
          label,
          List(pac),
          (optRsrc) => {
            optRsrc match {
              case None => ()
              case Some(mTT.RBoundHM(Some(mTT.Ground( v )), _)) => {
                v match {
                  case Bottom => fail("Undefined")
                  case PostedExpr( (PostedExpr( s : String ), _, _, _) ) => succeed(s)
                  case _ => fail("Unrecognized resource: " + optRsrc)
                }
              }
              case _ => fail("Unrecognized resource: " + optRsrc)
            }
          }
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      def withFilter(filter: String => Boolean): MonadT[U, String] = new MonadT[U, String] {
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

    case class Putter(value: String) extends MonadT[U, Unit] {
      private val putter = this
      def map[B](succeed: Unit => B): Unit = {
        EvalHandlerService.agentMgr().put(
          label,
          List(pac),
          value,
          (optRsrc) => {
            optRsrc match {
              case None => ()
              case Some(mTT.RBoundHM(Some(mTT.Ground( _ )), _)) => succeed()
              case _ => fail("Unrecognized resource: " + optRsrc)
            }
          }
        )
      }
      def unit[B](b: B): Unit = ()
      def flatten[B](mmb: Unit): Unit = ()
      
      def withFilter(filter: Unit => Boolean): MonadT[U, Unit] = new MonadT[U, Unit] {
        def map[B](succeed: Unit => B): Unit = putter.flatMap((u: Unit) => {
          if (filter()) { succeed() }
          ()
        })
        def unit[B](b: B): Unit = ()
        def flatten[B](mmb: Unit): Unit = ()
        def withFilter(filter: Unit => Boolean): MonadT[U, Unit] 
          = putter.withFilter(filter)
      }
    }

    def fetch() = Fetcher()
    def put(value: String) = Putter(value)
  }

  case class LoggingForwarderSlot[M[_]](
    target: SlotT[M, String],
    onFetch: String => Unit,
    onPut: String => Unit
  ) extends SlotT[M, String] {
    def fetch() = new MonadT[M, String] {
      val t = target.fetch()
      def map[B](f: String => B): M[B] = t.map((a: String) => { onFetch(a); f(a) })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
    def put(value: String) = new MonadT[M, Unit] {
      val t = target.put(value)
      def map[B](f: Unit => B): M[B] = t.map((x: Unit) => { onPut(value); f(value) })
      def flatten[B](mmb: M[M[B]]): M[B] = t.flatten(mmb)
      def unit[B](b: B): M[B] = t.unit(b)
    }
  }
  
  case class SerializingSlot[M[_], A <: java.io.Serializable](target: SlotT[M, String]) extends SlotT[M, A] {
    import biz.source_code.base64Coder.Base64Coder
    import java.io._
    
    def fetch() = new MonadT[M, A] {
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
    
    def put(value: A) = new MonadT[M, Unit] {
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
  }
}














