package com.biosimilarity.evaluator.spray.srp

import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.crypto.CryptoException
import org.bouncycastle.crypto.agreement.srp.{SRP6Server, SRP6StandardGroups, SRP6Util}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.prng.ThreadedSeedGenerator
import org.bouncycastle.util.encoders.Hex

object ConversionUtils {
  def fromHex (hex: String): BigInteger = new BigInteger(hex, 16)

  def toHex (bi: BigInteger): String = new String(Hex.encode(bi.toByteArray))

  def getRandomSalt: String = toHex(SRP6Util.generatePrivateValue(new SHA512Digest(),
                                    SRP6StandardGroups.rfc5054_1024.getN,
                                    SRP6StandardGroups.rfc5054_1024.getG, getSecureRandom))

  def getSecureRandom: SecureRandom = {
    val tsg: ThreadedSeedGenerator = new ThreadedSeedGenerator()
    val random = new SecureRandom()
    random.setSeed(tsg.generateSeed(20, true))
    random
  }
}

case class UserCredentials(I: String) extends SRP6Server {
  import ConversionUtils._

  def getLoginSessionKey(AHex: String): (String, String) = {
    generateServerCredentials()
    calculateSecret(fromHex(AHex))
    if (this.A == null || this.B == null || this.S == null) {
      throw new CryptoException("Impossible to compute and verify M1: " +
                                "some data are missing from the previous operations (A,B,S)")    }

    (toHex(SRP6Util.calculateM1(digest, N, A, B, S)), toHex(B))
  }

  def getM2Hex(M1Hex: String): String = toHex(SRP6Util.calculateM2(digest, N, A, fromHex(M1Hex), S))
}

object UserCredentials {
  import ConversionUtils._
  def apply(identity: String, verifierHex: String): UserCredentials = {
    val instance: UserCredentials = new UserCredentials(identity)
    instance.init(SRP6StandardGroups.rfc5054_1024, fromHex(verifierHex), new SHA512Digest(), getSecureRandom)
    instance
  }
}
