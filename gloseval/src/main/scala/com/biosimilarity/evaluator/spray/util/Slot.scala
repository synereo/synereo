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
    
    case class Fetcher() extends MonadT[U, String] with MFilter[U] {
      def flatMap[B](succeed: String => Unit): Unit = {
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
      def map[B](succeed: String => B): Unit = flatMap((x: String) => { succeed(x); () })
      def foreach[B](succeed: String => B): Unit = map(succeed)
    }

    case class Putter(value: String) extends MonadT[U, Unit] {
      def flatMap[B](succeed: Unit => Unit): Unit = {
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
      def map[B](succeed: Unit => B): Unit = flatMap((x: Unit) => { succeed(); () })
      def foreach[B](succeed: Unit => B): Unit = map(succeed)
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
      def flatMap[B](f: String => M[B]): M[B] = target.fetch().flatMap((a: String) => { onFetch(a); f(a) })
      def map[B](f: String => B): M[B] = target.fetch().map((a: String) => { onFetch(a); f(a) })
      def foreach[B](f: String => B): Unit = target.fetch().foreach((a: String) => { onFetch(a); f(a) })
    }
    def put(value: String) = new MonadT[M, Unit] {
      def flatMap[B](f: Unit => M[B]): M[B] = target.put(value).flatMap((x: Unit) => { onPut(value); f(value) })
      def map[B](f: Unit => B): M[B] = target.put(value).map((x: Unit) => { onPut(value); f(value) })
      def foreach[B](f: Unit => B): Unit = target.put(value).foreach((x: Unit) => { onPut(value); f(value) })
    }
  }
  
  case class SerializingSlot[M[_], A <: java.io.Serializable](target: SlotT[M, String]) extends SlotT[M, A] {
    import biz.source_code.base64Coder.Base64Coder
    import java.io._
    
    def fetch() = new MonadT[M, A] {
      def deserialize(s: String): A = {
        val data : Array[Byte] = Base64Coder.decode(s)
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[A]
      }
      def flatMap[B](f: A => M[B]): M[B] = target.fetch().flatMap(f compose deserialize)
      def map[B](f: A => B): M[B] = target.fetch().map(f compose deserialize)
      def foreach[B](f: A => B): Unit = target.fetch().foreach(f compose deserialize)
    }
    
    def put(value: A) = new MonadT[M, Unit] {
      def serialize[T <: java.io.Serializable](t: T): String = {
        val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos : ObjectOutputStream = new ObjectOutputStream( baos )
        oos.writeObject( t.asInstanceOf[java.io.Serializable] )
        oos.close()
        new String( Base64Coder.encode( baos.toByteArray() ) )
      }
      def flatMap[B](f: Unit => M[B]): M[B] = target.put(serialize(value)).flatMap(f)
      def map[B](f: Unit => B): M[B] = target.put(serialize(value)).map(f)
      def foreach[B](f: Unit => B): Unit = target.put(serialize(value)).foreach(f)
    }
  }
}














