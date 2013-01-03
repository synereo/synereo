package com.protegra_ati.agentservices.core.schema.persistence

import com.protegra_ati.agentservices.core.schema.Data
import com.protegra_ati.agentservices.core.schema.util.ReflectionHelper
import com.protegra_ati.agentservices.core.schema.Constants._
import com.protegra.agentservicesstore.extensions.StringExtensions._
import java.util.{Locale, UUID}

/* User: mgevantmakher
*/

trait StorableDataDefaults
{
  self: Data =>


  // Sets default values to be stored in the storage. Has to be overridden in each Data subclass where default values are nulls or string coded booleans etc.
  // Be careful, this breaks immutability, after this method is called, the object on which this method was called cant be reused for something else as to store data
  // TODO may be it worse it to create a copy/deep clone of the object in a toSearchKey on the fly for one way use to build key to prevent a immutability break
  //tests should be the only ones accessing this directly outside of toStorekey
  def setDefaultValues(isChild: Boolean) =
  {
    if ( isChild ) {
      id = ""
      localeCode = ""
      recVerNum = ""
    }
    else {
      // Double check prevents storing of the key without essential data
      if ( id == null || id.isEmpty ) id = UUID.randomUUID().toString()
      if ( localeCode == null || localeCode.isEmpty ) localeCode = Locale.ENGLISH.toString()
      if ( recVerNum == null || recVerNum.isEmpty ) recVerNum = "1"
    }
  }

}