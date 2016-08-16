package com.biosimilarity.evaluator.spray.srp

import org.bouncycastle.crypto.agreement.srp.{SRP6StandardGroups, SRP6VerifierGenerator}
import org.bouncycastle.crypto.digests.SHA512Digest
import ConversionUtils._

object VerifierGenerator {

  def generateVerifier(identity: String, password: String, salt: String): String = {
    val verifierGenerator = new SRP6VerifierGenerator()
    verifierGenerator.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest())
    toHex(verifierGenerator.generateVerifier(salt.getBytes, identity.getBytes, password.getBytes))
  }

}
