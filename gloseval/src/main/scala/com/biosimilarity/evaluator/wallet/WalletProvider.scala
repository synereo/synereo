package com.biosimilarity.evaluator.wallet

import java.io.File
import java.net.URI

import com.biosimilarity.evaluator.spray.SessionActor.StopKit
import com.biosimilarity.evaluator.spray.{SessionActor, SessionManager}
import org.bitcoinj.core.listeners.TransactionConfidenceEventListener
import org.bitcoinj.core.{Address, Coin, Transaction}
import org.bitcoinj.kits.WalletAppKit
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import org.bitcoinj.wallet.listeners.{WalletCoinsReceivedEventListener, WalletCoinsSentEventListener}

object WalletProvider {

  def createNewWallet: Wallet = new Wallet(Network.params)

  def restoreWallet(seedData: DeterministicSeedData, sessionURI: String,
                    onreceived: (Wallet, Transaction, Coin, Coin) => Unit,
                    onsent: (Wallet, Transaction, Coin, Coin) => Unit,
                    onconfidencechanged: (Wallet, Transaction) => Unit): WalletAppKit = {

    val session = new URI(sessionURI).getHost
    stopKitInSession(session)
    println(s"----------------------------------------> Wallet for session: $session")
    val kit: WalletAppKit = new WalletAppKit(Network.params, new File("."), s"restorewallet-$session")
    kit.restoreWalletFromSeed(new DeterministicSeed(seedData.seedcode, null, seedData.passphrase, seedData.creationtime))
    kit.startAsync
    kit.awaitRunning

    kit.wallet.addCoinsReceivedEventListener(new WalletCoinsReceivedEventListener() {
      override def onCoinsReceived(wallet: Wallet, tx: Transaction, prevBalance: Coin, newBalance: Coin): Unit = {
        onreceived(wallet, tx, prevBalance, newBalance)
      }
    })

    kit.wallet.addCoinsSentEventListener(new WalletCoinsSentEventListener() {
      override def onCoinsSent(wallet: Wallet, tx: Transaction, prevBalance: Coin, newBalance: Coin): Unit = {
        onsent(wallet, tx, prevBalance, newBalance)
      }
    })

    kit.wallet.addTransactionConfidenceEventListener(new TransactionConfidenceEventListener() {
      override def onTransactionConfidenceChanged(wallet: Wallet, tx: Transaction): Unit = {
        onconfidencechanged(wallet, tx)
      }
    })

    println("send money to: " + kit.wallet.freshReceiveAddress.toString)

    kit
  }

  def getReceiveAddress(seedData: DeterministicSeedData): Address = {
    val seed = new DeterministicSeed(seedData.seedcode, null, seedData.passphrase, seedData.creationtime)
    Wallet.fromSeed(Network.params, seed).currentReceiveAddress()
  }

  private def stopKitInSession(session: String): Unit = {
    SessionManager.getSessionByHost(session) match {
      case Some(actor) => actor ! StopKit
      case None =>
    }
  }
}
