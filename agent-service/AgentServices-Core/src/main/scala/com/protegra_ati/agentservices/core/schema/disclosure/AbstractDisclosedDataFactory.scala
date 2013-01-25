package com.protegra_ati.agentservices.core.schema.disclosure

import com.protegra_ati.agentservices.core.schema._
import com.protegra_ati.agentservices.store.util._
import com.protegra_ati.agentservices.store.util.Reporting


/* User: mgevantmakher
*/

trait AbstractDisclosedDataFactory extends Reporting
{
  protected def getDefaultDisclosedDataCollection(): scala.collection.immutable.Map[ TrustLevel.Value, DisclosedData[ _ <: Data ] ]

  def getDefaultDisclosedData(trustLevel: TrustLevel.Value): DisclosedData[ _ <: Data ] =
  {
    getDefaultDisclosedDataCollection().get(trustLevel) match {
      case None => {
        // should never happen
        report("cannot find default DisclosedData for " + trustLevel.toString + " in " + this.getClass.getName, Severity.Error)
        null
      }
      case Some(defaultDisclosedData) => {
        defaultDisclosedData
      }
    }
  }
}