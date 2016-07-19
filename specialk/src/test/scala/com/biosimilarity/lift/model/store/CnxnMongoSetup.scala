package com.biosimilarity.lift.model.store

import scala.language.implicitConversions

object CnxnMongoSetup extends JSONIfy[String, String, String] with Serializable {

  object MyCCLConversions extends Serializable {

    implicit def toConverter(ccl: CnxnCtxtLabel[String, String, String]): CCLConversionsWrapper =
      CCLConversionsWrapper(ccl, identity, identity, identity)

    def apply(ccl: CnxnCtxtLabel[String, String, String]): CCLConversionsWrapper =
      CCLConversionsWrapper(ccl, identity, identity, identity)
  }
}
