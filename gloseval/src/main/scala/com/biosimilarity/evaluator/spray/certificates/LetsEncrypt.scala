package com.biosimilarity.evaluator.spray.certificates

import java.io.{File, FileReader, FileWriter}
import java.net.URI
import java.nio.file.{Files, Path}
import java.security.cert.X509Certificate
import java.security.{KeyPair, Security}
import javax.swing.JOptionPane

import com.biosimilarity.evaluator.spray.{Boot, BootTasks}
import com.biosimilarity.evaluator.util.{withFileReader, withFileWriter}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.shredzone.acme4j._
import org.shredzone.acme4j.challenge.Http01Challenge
import org.shredzone.acme4j.exception.{AcmeConflictException, AcmeUnauthorizedException}
import org.shredzone.acme4j.util.{CSRBuilder, CertificateUtils, KeyPairUtils}
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

/**
  * Adapted from:
  * [[https://github.com/shred/acme4j/blob/master/acme4j-example/src/main/java/org/shredzone/acme4j/ClientTest.java]]
  */
object LetsEncrypt {

  private val USER_KEY_FILE: File    = new File("user.key")
  private val DOMAIN_KEY_FILE: File  = new File("domain.key")
  private val DOMAIN_CERT_FILE: File = new File("domain.crt")
  private val CERT_CHAIN_FILE: File  = new File("chain.crt")
  private val DOMAIN_CSR_FILE: File  = new File("domain.csr")

  private val KEY_SIZE: Int = 2048

  private val LOG: Logger = LoggerFactory.getLogger(classOf[BootTasks])

  Security.addProvider(new BouncyCastleProvider)

  private def acceptAgreement(reg: Registration, agreement: URI): Try[Unit] =
    Try(reg.modify().setAgreement(agreement).commit())

  private def findHttpChallenge(domain: String, auth: Authorization): Try[Http01Challenge] =
    Try(auth.findChallenge(Http01Challenge.TYPE): Http01Challenge)

  private def readKeyPair(file: File): Try[(KeyPair, Boolean)] =
    Try {
      withFileReader[(KeyPair, Boolean)](file) { (fr: FileReader) =>
        (KeyPairUtils.readKeyPair(fr), false)
      }
    }

  private def createUserKeyPair(file: File, keySize: Int): Try[(KeyPair, Boolean)] =
    Try {
      val userKeyPair = KeyPairUtils.createKeyPair(keySize)
      withFileWriter[Unit](file) { (fw: FileWriter) =>
        KeyPairUtils.writeKeyPair(userKeyPair, fw)
      }
      (userKeyPair, true)
    }

  private def readOrCreateUserKeyPair(file: File, keySize: Int = KEY_SIZE): Try[(KeyPair, Boolean)] =
    if (file.exists())
      readKeyPair(file)
    else
      createUserKeyPair(file, keySize)

  private def createSession(userKeyPair: KeyPair): Try[Session] =
    Try(new Session("acme://letsencrypt.org/staging", userKeyPair))

  private def registerNewUser(session: Session): Try[Registration] =
    Try {
      val registration: Registration = new RegistrationBuilder().create(session)
      LOG.info(s"Registered a new user, URI: ${registration.getLocation}")
      registration
    }.recoverWith {
      case ex: AcmeConflictException =>
        Try {
          val registration: Registration = Registration.bind(session, ex.getLocation)
          LOG.info(s"Account does already exist, URI: ${registration.getLocation}")
          registration
        }
    }

  private def getAgreement(reg: Registration): Try[URI] =
    Try {
      val uri: URI = reg.getAgreement
      LOG.info(s"Terms of Service: $uri")
      uri
    }

  private def acceptAgreement(createdNewKeyPair: Boolean, reg: => Registration, agreement: => URI): Try[Unit] =
    Try {
      if (createdNewKeyPair) {
        acceptAgreement(reg, agreement)
      }
    }

  private def createAuthorization(domain: String, reg: Registration, agreement: URI): Try[Authorization] =
    Try {
      reg.authorizeDomain(domain)
    }.recoverWith {
      case ex: AcmeUnauthorizedException =>
        Try {
          // Maybe there are new T&C to accept?
          acceptAgreement(true, reg, agreement)
          // Then try again...
          reg.authorizeDomain(domain)
        }
    }

  private def handleChallengeManually(domain: String, challenge: Http01Challenge): Try[Unit] =
    Try {
      LOG.info("Please create a file in your web server's base directory.")
      LOG.info("It must be reachable at: http://" + domain + "/.well-known/acme-challenge/" + challenge.getToken)
      LOG.info("File name: " + challenge.getToken)
      LOG.info("Content: " + challenge.getAuthorization)
      LOG.info("The file must not contain any leading or trailing whitespaces or line breaks!")
      LOG.info("If you're ready, dismiss the dialog...")

      val message: StringBuilder = new StringBuilder
      message.append("Please create a file in your web server's base directory.\n\n")
      message.append("http://").append(domain).append("/.well-known/acme-challenge/").append(challenge.getToken).append("\n\n")
      message.append("Content:\n\n")
      message.append(challenge.getAuthorization)
      val option: Int = JOptionPane.showConfirmDialog(null, message.toString(), "Prepare Challenge", JOptionPane.OK_CANCEL_OPTION)
      if (option == JOptionPane.CANCEL_OPTION) {
        LOG.error("User cancelled challenge")
        System.exit(1)
      }
      ()
    }

  private def handleChallenge(domain: String, challenge: Http01Challenge): Try[File] =
    Try {
      val challengeFile: Path = Boot.pwd.resolve(s"challenge/.well-known/acme-challenge/${challenge.getToken}")
      val _: Path             = Files.createDirectories(challengeFile.getParent)
      withFileWriter[Unit](challengeFile.toFile) { (fw: FileWriter) =>
        fw.write(challenge.getAuthorization)
      }
      challengeFile.toFile
    }

  // Poll for the challenge to complete
  private def completeChallenge(challenge: Http01Challenge): Try[Unit] =
    Try {
      challenge.trigger()
      var attempts: Int = 10
      while (challenge.getStatus != Status.VALID && attempts > 0) {
        attempts -= 1
        if (challenge.getStatus == Status.INVALID) {
          LOG.error("Challenge failed... Giving up.")
          System.exit(1)
        }
        try {
          Thread.sleep(3000L)
        } catch {
          case ex: InterruptedException =>
            LOG.warn("interruped", ex)
        }
        challenge.update()
        if (challenge.getStatus != Status.VALID) {
          LOG.error("Failed to pass the challenge... Giving up.")
          System.exit(1)
        }
      }
    }

  private def readDomainKeyPair(file: File): Try[KeyPair] =
    Try {
      withFileReader[KeyPair](file) { (fr: FileReader) =>
        KeyPairUtils.readKeyPair(fr)
      }
    }

  private def createDomainKeyPair(file: File, keySize: Int): Try[KeyPair] =
    Try {
      withFileWriter[KeyPair](file) { (fw: FileWriter) =>
        val domainKeyPair: KeyPair = KeyPairUtils.createKeyPair(keySize)
        KeyPairUtils.writeKeyPair(domainKeyPair, fw)
        domainKeyPair
      }
    }

  private def readOrCreateDomainKeyPair(file: File, keySize: Int = KEY_SIZE): Try[KeyPair] =
    if (file.exists())
      readDomainKeyPair(file)
    else
      createDomainKeyPair(file, keySize)

  private def generateCSR(domain: String, domainKeyPair: KeyPair): Try[CSRBuilder] =
    Try {
      val csrb: CSRBuilder = new CSRBuilder()
      csrb.addDomain(domain)
      csrb.sign(domainKeyPair)
      csrb
    }

  private def requestSignedCertificate(reg: Registration, csrb: CSRBuilder): Try[Certificate] =
    Try(reg.requestCertificate(csrb.getEncoded))

  private def downloadCertificate(file: File, certificate: Certificate): Try[File] =
    Try {
      withFileWriter[File](file) { (fw: FileWriter) =>
        val cert: X509Certificate = certificate.download()
        CertificateUtils.writeX509Certificate(cert, fw)
        file
      }
    }

  private def downloadCertificateChain(file: File, certificate: Certificate): Try[File] =
    Try {
      withFileWriter[File](file) { (fw: FileWriter) =>
        val chain: Array[X509Certificate] = certificate.downloadChain()
        CertificateUtils.writeX509CertificateChain(chain, fw)
        file
      }
    }

  def createChallenge(domain: String): Try[(File, Http01Challenge, Registration)] =
    for {
      (userKeyPair, createdNewKeyPair) <- readOrCreateUserKeyPair(USER_KEY_FILE)
      session                          <- createSession(userKeyPair)
      registration                     <- registerNewUser(session)
      agreement                        <- getAgreement(registration)
      _                                <- acceptAgreement(createdNewKeyPair, registration, agreement)
      authorization                    <- createAuthorization(domain, registration, agreement)
      _                                <- Try(LOG.info(s"New authorization for domain: $domain"))
      _                                <- Try(LOG.info(s"Authorization is: $authorization"))
      challenge                        <- findHttpChallenge(domain, authorization)
      challengeFile                    <- handleChallenge(domain, challenge)
    } yield (challengeFile, challenge, registration)

  def awaitResponse(domain: String, challenge: Http01Challenge, registration: Registration): Try[(File, File)] =
    for {
      _                    <- completeChallenge(challenge)
      domainKeyPair        <- readOrCreateDomainKeyPair(DOMAIN_KEY_FILE)
      csrBuilder           <- generateCSR(domain, domainKeyPair)
      certificate          <- requestSignedCertificate(registration, csrBuilder)
      _                    <- Try(LOG.info(s"Success! The certificate for domain $domain has been generated!"))
      _                    <- Try(certificate.getLocation)
      certificateFile      <- downloadCertificate(DOMAIN_CERT_FILE, certificate)
      certificateChainFile <- downloadCertificateChain(CERT_CHAIN_FILE, certificate)
    } yield (certificateFile, certificateChainFile)

  /**
    * Generates a certificate for the given domains. Also takes care for the registration
    * process.
    *
    * @param domain Domain to get a common certificate for
    */
  def fetchCertificate(domain: String): Try[(File, File)] =
    for {
      (userKeyPair, createdNewKeyPair) <- readOrCreateUserKeyPair(USER_KEY_FILE)
      session                          <- createSession(userKeyPair)
      registration                     <- registerNewUser(session)
      agreement                        <- getAgreement(registration)
      _                                <- acceptAgreement(createdNewKeyPair, registration, agreement)
      authorization                    <- createAuthorization(domain, registration, agreement)
      _                                <- Try(LOG.info(s"New authorization for domain: $domain"))
      _                                <- Try(LOG.info(s"Authorization is: $authorization"))
      challenge                        <- findHttpChallenge(domain, authorization)
      _                                <- handleChallengeManually(domain, challenge)
      _                                <- completeChallenge(challenge)
      domainKeyPair                    <- readOrCreateDomainKeyPair(DOMAIN_KEY_FILE)
      csrBuilder                       <- generateCSR(domain, domainKeyPair)
      certificate                      <- requestSignedCertificate(registration, csrBuilder)
      _                                <- Try(LOG.info(s"Success! The certificate for domain $domain has been generated!"))
      _                                <- Try(certificate.getLocation)
      certificateFile                  <- downloadCertificate(DOMAIN_CERT_FILE, certificate)
      certificateChainFile             <- downloadCertificateChain(CERT_CHAIN_FILE, certificate)
    } yield (certificateFile, certificateChainFile)
}
