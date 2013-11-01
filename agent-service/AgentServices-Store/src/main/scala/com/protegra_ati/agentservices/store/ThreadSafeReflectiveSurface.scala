package com.protegra_ati.agentservices.store

import com.biosimilarity.lift.model.store.ReflectiveSurfaceT
import java.util.UUID
import scala.reflect.runtime.universe._

trait ThreadSafeReflectiveSurfaceT {
  private val _lockObj: AnyRef = new Object()
  private var _lockId: Option[UUID] = None
  private val _rs = new ReflectiveSurfaceT {}

  case class Specimen(
    originalType: Type,
    generatingExpression: Option[Any],
    contents: AnyRef
  )

  def lock(f: UUID => Unit): Unit = {
    _lockObj.synchronized {
      val uuid = UUID.randomUUID()
      _lockId = Some(uuid)
      f(uuid)
      _lockId = None
    }
  }

  def captureSpecimenAs[T: TypeTag](lockId: UUID, content: T): Specimen = {
    _lockId match {
      case Some(x) if x == lockId => Specimen(_rs.getType(content), None, content.asInstanceOf[AnyRef])
      case _ => throw new Exception("Must wrap captureSpecimenAs in ThreadSafeReflectiveSurface.lock")
    }
  }
}

object ThreadSafeReflectiveSurface extends ThreadSafeReflectiveSurfaceT

package threadSafeReflectiveSurface.usage {
  case class Looky[N](b: Boolean, s: String, n: N)

  object TryOut {
    def test(): Unit = {
      var specimen: Option[ThreadSafeReflectiveSurface.Specimen] = None

      ThreadSafeReflectiveSurface.lock { (lockId: UUID) =>
        specimen = Some(ThreadSafeReflectiveSurface.captureSpecimenAs(lockId, Looky(true, "string", 5)))
      }

      println(specimen)
    }
  }
}
