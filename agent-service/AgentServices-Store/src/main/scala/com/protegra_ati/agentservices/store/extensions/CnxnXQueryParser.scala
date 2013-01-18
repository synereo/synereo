//jsk - kept this separate to keep the StringExtensions less cluttered with withs
//using a trait so it doesnt get called as a static object except through extensions

package com.protegra_ati.agentservices.store.extensions

import com.biosimilarity.lift.lib.UUIDOps
import com.biosimilarity.lift.model.store._

trait CnxnXQueryParser
 extends CnxnXQuery[String,String,String]
 with UUIDOps
 with Blobify
 with CnxnCtxtInjector[String,String,String]
 with CnxnXML[String,String,String]
{
  def labelFromKey(key:String): CnxnCtxtLabel[String,String,String] = {
    val rawLbl = fromCaseClassInstanceString(key).getOrElse(null)
    rawLbl.asInstanceOf[CnxnCtxtLabel[String,String,String]]
  }
}
