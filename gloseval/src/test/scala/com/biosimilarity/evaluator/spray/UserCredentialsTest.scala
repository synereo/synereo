package com.biosimilarity.evaluator.spray

import java.security.SecureRandom

import com.biosimilarity.evaluator.spray.srp.ConversionUtils._
import com.biosimilarity.evaluator.spray.srp.UserCredentials
import org.bouncycastle.crypto.agreement.srp.{SRP6Client, SRP6StandardGroups, SRP6VerifierGenerator}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class UserCredentialsTest  extends FlatSpec with Matchers {
  val testusername = "username"
  val testpass1 = "qwerty"
  val testpass2 = "password"

  "SRP implementation" should "store password verifier and make the verification on server and client side" in {
    val dbData = registration(testusername, testpass1)

    dbData.nonEmpty should be (true)
    Try(dbData.split(":")).toOption.isDefined should be (true)
    login(testusername, testpass1, dbData) should be (true)
    login(testusername, testpass2, dbData) should be (false)
  }

  def registration(username: String, password: String): String = {
    // Registration
    // server generates random salt and sends it to the client
    val saltHex = getRandomSalt

    // client generates the verifier from the username, password and given salt
    val vg: SRP6VerifierGenerator = new SRP6VerifierGenerator()
    vg.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest())
    val verifierHex = toHex(vg.generateVerifier(saltHex.getBytes, username.getBytes, password.getBytes))

    s"$saltHex:$verifierHex"
  }

  def login(username: String, password: String, dbData: String): Boolean = {
    Try(dbData.split(":")).toOption match {
      case None => false
      case Some(arr) =>
        val (salt, verifier) = (arr(0), fromHex(arr(1)))
        // Login
        val srpc: SRP6Client = new SRP6Client()
        srpc.init(SRP6StandardGroups.rfc5054_1024, new SHA512Digest(), new SecureRandom())

        // client calculates A
        val AHex = toHex(srpc.generateClientCredentials(salt.getBytes, username.getBytes(), password.getBytes))

        // server calculates B and M1
        val uc: UserCredentials = UserCredentials(testusername, toHex(verifier))
        val (serverM1Hex, bhex) = uc.getLoginSessionKey(AHex)

        // client calculates M1
        srpc.calculateSecret(fromHex(bhex))
        val clientM1Hex = toHex(srpc.calculateClientEvidenceMessage())

        println(s"server M1: $serverM1Hex")
        println(s"client M1: $clientM1Hex")

        // verification on server
        if (serverM1Hex.equals(clientM1Hex)) {
          // server calculates M2
          val serverM2Hex = uc.getM2Hex(serverM1Hex)
          // verification on client
          srpc.verifyServerEvidenceMessage(fromHex(serverM2Hex))
        } else false
    }
  }
}
