package com.protegra_ati.agentservices.core.schema

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import java.util.UUID
import scala.reflect.BeanProperty
import com.protegra_ati.agentservices.core.schema.validator._

case class VerifiedData(@BeanProperty var alias:String, @BeanProperty var key:String, @BeanProperty var value:String) extends Data
with VerifiedDataValidator{

  //need a no-parameter constructor to create a version of the object with only authorized fields populated
  def this() = this("", "", "")

  override def toString = {
    "verified data(key:" + key + ", value:" + value + ")"
  }


}
