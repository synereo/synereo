package com.protegra_ati.agentservices.core.schema

/**
 * Used to mark certain Data-types that have another dependent Data-types (Parent-Child relation).
 * This trait a should keep the generic way of search for dependent objects.
 * The relationship between parent (this) and child (dependent) objects works over childId, which is corresponding to the parentId in the ParentData trait
 */
trait ChildData
{
  /**
   * returns the childId it is like foreign key links parent and child objects
   * @return
   */
  def getChildId(): String

}
