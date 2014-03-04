package com.biosimilarity.evaluator.spray.util

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn

import com.biosimilarity.lift.model.store._

object Slot extends Serializable {
  trait SlotT[M[_], T] {
    def fetch(): MonadT[M, T]
    def put(value: T): MonadT[M, Unit]
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
    
    case class Fetcher(val filter: String => Boolean = { (x) => true }) extends MonadT[U, String] {
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
                  case PostedExpr( (PostedExpr( s : String ), _, _, _) ) => if (filter(s)) { succeed(s) }
                  case _ => fail("Unrecognized resource: " + optRsrc)
                }
              }
              case _ => fail("Unrecognized resource: " + optRsrc)
            }
          }
        )
      }

      def map[B](succeed: String => B): Unit = flatMap((x) => { succeed(x); () })

      def foreach[B](succeed: String => B): Unit = map(succeed)

      def withFilter(test: String => Boolean): MonadT[U, String] = Fetcher(test)
    }

    case class Putter(value: String, val filter: Unit => Boolean = { (x) => true }) extends MonadT[U, Unit] {
      def flatMap[B](succeed: Unit => Unit): Unit = {
        EvalHandlerService.agentMgr().put(
          label,
          List(pac),
          value,
          (optRsrc) => {
            optRsrc match {
              case None => ()
              case Some(mTT.RBoundHM(Some(mTT.Ground( _ )), _)) => if (filter()) { succeed() }
              case _ => fail("Unrecognized resource: " + optRsrc)
            }
          }
        )
      }

      def map[B](succeed: Unit => B): Unit = flatMap((x) => { succeed(x); () })

      def foreach[B](succeed: Unit => B): Unit = map(succeed)

      def withFilter(test: Unit => Boolean): MonadT[U, Unit] = Putter(value, test)
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
      // def withFilter(f: String => Boolean): MonadT[M, String] = target.fetch().withFilter(f)
    }
    def put(value: String) = new MonadT[M, Unit] {
      def flatMap[B](f: Unit => M[B]): M[B] = target.put(value).flatMap(Unit => { onPut(value); f(value) })
      def map[B](f: Unit => B): M[B] = target.put(value).map(Unit => { onPut(value); f(value) })
      def foreach[B](f: Unit => B): Unit = target.put(value).foreach(Unit => { onPut(value); f(value) })
      // def withFilter(f: Unit => Boolean): MonadT[M, Unit] = target.put(value).withFilter(f)
    }
  }
  
  case class SerializingSlot[M[_], T <: Serializable](target: SlotT[M, String]) extends SlotT[M, T] {
    import biz.source_code.base64Coder.Base64Coder
    import java.io._
    
    def fetch() = new MonadT[M, T] {
      def deserialize(s: String): T = {
        val data : Array[Byte] = Base64Coder.decode(s)
        val ois : ObjectInputStream = new ObjectInputStream( new ByteArrayInputStream(  data ) )
        val o : java.lang.Object = ois.readObject()
        ois.close()
        o.asInstanceOf[T]
      }
      def flatMap[B](f: T => M[B]): M[B] = target.fetch().flatMap(f compose deserialize)
      def map[B](f: T => B): M[B] = target.fetch().map(f compose deserialize)
      def foreach[B](f: T => B): Unit = target.fetch().foreach(f compose deserialize)
      // def withFilter(f: T => Boolean): MonadT[M, T] = target.fetch().withFilter(f compose deserialize)
    }
    
    def put(value: T) = new MonadT[M, Unit] {
      def serialize(t: T): String = {
        val baos : ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos : ObjectOutputStream = new ObjectOutputStream( baos )
        oos.writeObject( t.asInstanceOf[Serializable] )
        oos.close()
        new String( Base64Coder.encode( baos.toByteArray() ) )
      }
      def flatMap[B](f: Unit => M[B]): M[B] = target.put(serialize(value)).flatMap(f)
      def map[B](f: Unit => B): M[B] = target.put(serialize(value)).map(f)
      def foreach[B](f: Unit => B): Unit = target.put(serialize(value)).foreach(f)
      // def withFilter(f: Unit => Boolean): MonadT[M, Unit] = target.put(serialize(value)).withFilter(f)
    }
  }
}














