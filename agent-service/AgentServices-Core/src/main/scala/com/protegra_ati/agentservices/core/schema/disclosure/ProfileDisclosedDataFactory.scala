package com.protegra_ati.agentservices.core.schema.disclosure

import com.protegra_ati.agentservices.core.schema._
import scala.collection.immutable.HashMap

/* User: mgevantmakher
*/

class ProfileDisclosedDataFactory private() extends AbstractDisclosedDataFactory
{
  private val defaultDisclosedDataCollection: scala.collection.immutable.HashMap[ TrustLevel.Value, DisclosedData[ Profile ] ] =
    HashMap[ TrustLevel.Value, DisclosedData[ Profile ] ](
      TrustLevel.Full -> DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Full.toString, "id,localeCode,firstName,lastName,description,emailAddress,country,region,city,postalCode,website,imageHashCode"),
      TrustLevel.Basic -> DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Basic.toString, "id,localeCode,firstName,lastName,country,imageHashCode"),
      TrustLevel.Introduced -> DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Introduced.toString, "id,localeCode,lastName,country"),
      TrustLevel.Empty -> DisclosedData[ Profile ](classOf[ Profile ], TrustLevel.Empty.toString, ""))

  override protected def getDefaultDisclosedDataCollection(): scala.collection.immutable.Map[ TrustLevel.Value, DisclosedData[ _ <: Data ] ] =
  {
    defaultDisclosedDataCollection
  }
}

object ProfileDisclosedDataFactory
{
  private val theFactory: ProfileDisclosedDataFactory = new ProfileDisclosedDataFactory()

  def getDisclosedData(trustLevel: TrustLevel.Value): DisclosedData[ Profile ] =
  {
    theFactory.getDefaultDisclosedData(trustLevel).asInstanceOf[ DisclosedData[ Profile ] ]
  }
}