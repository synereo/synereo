package com.protegra_ati.agentservices.core.schema.disclosure

import com.protegra_ati.agentservices.core.schema._
import scala.collection.immutable.HashMap

/* User: mgevantmakher
*/

class AppIdDisclosedDataFactory private() extends AbstractDisclosedDataFactory
{
  private val defaultDisclosedDataCollection: scala.collection.immutable.HashMap[ TrustLevel.Value, DisclosedData[ AppId ] ] =
    HashMap[ TrustLevel.Value, DisclosedData[ AppId ] ](
      TrustLevel.Full -> DisclosedData[ AppId ](classOf[ AppId ], TrustLevel.Full.toString, "id,localeCode,name"),
      TrustLevel.Basic -> DisclosedData[ AppId ](classOf[ AppId ], TrustLevel.Basic.toString, "id,localeCode,name"),
      TrustLevel.Introduced -> DisclosedData[ AppId ](classOf[ AppId ], TrustLevel.Introduced.toString, "id,localeCode,name"),
      TrustLevel.Empty -> DisclosedData[ AppId ](classOf[ AppId ], TrustLevel.Empty.toString, "id,localeCode"))  // TODO check if ""/empty has to be used

  private val adminDisclosedData = DisclosedData[ AppId ](classOf[ AppId ], "admin", "id,localeCode,name")

  override protected def getDefaultDisclosedDataCollection(): scala.collection.immutable.Map[ TrustLevel.Value, DisclosedData[ _ <: Data ] ] =
  {
    defaultDisclosedDataCollection
  }
}

object AppIdDisclosedDataFactory
{
  private val theFactory: AppIdDisclosedDataFactory = new AppIdDisclosedDataFactory()

  def getDisclosedData(trustLevel: TrustLevel.Value): DisclosedData[ AppId ] =
  {
    theFactory.getDefaultDisclosedData(trustLevel).asInstanceOf[ DisclosedData[ AppId ] ]
  }

  def getAdminDisclosedData() =
  {
    theFactory.adminDisclosedData
  }
}
