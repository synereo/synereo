package com.protegra_ati.agentservices.core.schema.disclosure

import com.protegra_ati.agentservices.core.schema._
import scala.collection.immutable.HashMap

class MockProfileDisclosedDataFactory private() extends AbstractDisclosedDataFactory
{
  private val defaultDisclosedDataCollection: scala.collection.immutable.HashMap[ TrustLevel.Value, DisclosedData[ MockProfile ] ] =
    HashMap[ TrustLevel.Value, DisclosedData[ MockProfile ] ](
      TrustLevel.Full -> DisclosedData[ MockProfile ](classOf[ MockProfile ], TrustLevel.Full.toString, "id,localeCode,firstName,lastName,description,emailAddress,country,region,city,postalCode,website"),
      TrustLevel.Basic -> DisclosedData[ MockProfile ](classOf[ MockProfile ], TrustLevel.Basic.toString, "id,localeCode,firstName,lastName,image"),
      TrustLevel.Introduced -> DisclosedData[ MockProfile ](classOf[ MockProfile ], TrustLevel.Introduced.toString, "id,localeCode,lastName"),
      TrustLevel.Empty -> DisclosedData[ MockProfile ](classOf[ MockProfile ], TrustLevel.Empty.toString, ""))

  override protected def getDefaultDisclosedDataCollection(): scala.collection.immutable.Map[ TrustLevel.Value, DisclosedData[ _ <: Data ] ] =
  {
    defaultDisclosedDataCollection
  }
}

object MockProfileDisclosedDataFactory
{
  private val theFactory: MockProfileDisclosedDataFactory = new MockProfileDisclosedDataFactory()

  def getDisclosedData(trustLevel: TrustLevel.Value): DisclosedData[ MockProfile ] =
  {
    theFactory.getDefaultDisclosedData(trustLevel).asInstanceOf[ DisclosedData[ MockProfile ] ]
  }
}