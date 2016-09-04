package com.biosimilarity.evaluator.spray.srp

import java.math.BigInteger

import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import org.bouncycastle.crypto.CryptoException
import org.bouncycastle.crypto.agreement.srp.{SRP6Client, SRP6StandardGroups, SRP6Util}
import org.bouncycastle.crypto.digests.SHA512Digest

class SRPClient extends SRP6Client {
  def init: Unit = this.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest(), getSecureRandom)

  def calculateAHex: String  = {
    this.a = selectPrivateValue
    this.A = g.modPow(a, N)
    toHex(A)
  }

  def calculateMHex(BHex: String): String = {
    calculateSecret(fromHex(BHex))
    toHex(calculateClientEvidenceMessage)
  }

  def generateVerifier: String = {
    if(Option(this.x).isEmpty) {
      throw new CryptoException("Impossible to compute verifier: X value is missing.")
    }

    toHex(g.modPow(x, N))
  }

  def calculateX(identity: String, password: String, salt: String) = {
    if(Option(identity).isEmpty || Option(password).isEmpty || Option(salt).isEmpty) {
      throw new CryptoException("Impossible to compute X: identity or password or salt value is missing.")
    }
    val inner = hash(s"$identity:$password".getBytes)
    val result = SRP6Util.calculateU(digest, N, fromHex(salt), inner)
    this.x = result
  }

  def hash(data: Array[Byte]): BigInteger = {
    val digest = new SHA512Digest()
    val output = new Array[Byte](digest.getDigestSize)
    digest.update(data, 0, data.length)
    digest.doFinal(output, 0)
    new BigInteger(1, output)
  }

}