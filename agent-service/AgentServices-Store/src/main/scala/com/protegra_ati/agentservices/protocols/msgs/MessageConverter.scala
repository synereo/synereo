package com.protegra_ati.agentservices.protocols.msgs

import com.protegra_ati.agentservices.store.ThreadSafeReflectiveSurface
import java.util.UUID

object MessageConverter {
  def beginIntroductionRequestToSpecimen(birq: BeginIntroductionRequest): ThreadSafeReflectiveSurface.Specimen = {
    var specimen: ThreadSafeReflectiveSurface.Specimen = null

    ThreadSafeReflectiveSurface.lock { (lockId: UUID) =>
      specimen = ThreadSafeReflectiveSurface.captureSpecimenAs(lockId, birq)
    }

    specimen
  }
}
