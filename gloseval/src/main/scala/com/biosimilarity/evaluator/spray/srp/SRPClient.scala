package com.biosimilarity.evaluator.spray.srp

import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import org.bouncycastle.crypto.agreement.srp.{SRP6Client, SRP6StandardGroups, SRP6Util}
import org.bouncycastle.crypto.digests.SHA512Digest

class SRPClient extends SRP6Client {
  def init: Unit = this.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest(), getSecureRandom)
  def calculateAHex: String  = {
    this.a = selectPrivateValue
    this.A = g.modPow(a, N)
    toHex(A)
  }
  def calculateX(salt: String, identity: String, password: String) = {
    this.x = SRP6Util.calculateX(digest, N, salt.getBytes(), identity.getBytes(), password.getBytes())
  }
  def calculateMHex(BHex: String): String = {
    calculateSecret(fromHex(BHex))
    toHex(calculateClientEvidenceMessage)
  }

}