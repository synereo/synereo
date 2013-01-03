package com.protegra_ati.agentservices.core.schema.persistence

import com.protegra_ati.agentservices.core.schema.{ChildData, Data}

/* User: mgevantmakher
*/


trait SearchableChildData extends ChildData
{
  /**
   * returns toSearchKeys for all child objects to be used for search
   * @return list of the strings used for the search for all child's
   */
  def getChildDataSearchKeys: java.util.List[ Data ]

  /**
   *  This method has to be overridden in a implementing class to inject specific child data search results to a related- data result list
   * @return default child data list.
   */
  def getDefaultChildData(): List[ Data ] =
  {
    Nil
  }

}
