package com.biosimilarity.evaluator.spray.wallet

import java.io.File
import java.net.URI

import com.biosimilarity.evaluator.spray.SessionManager
import org.bitcoinj.core.listeners.{DownloadProgressTracker, TransactionConfidenceEventListener}
import org.bitcoinj.core.{Address, Coin, Transaction}
import org.bitcoinj.kits.WalletAppKit
import org.bitcoinj.wallet.{DeterministicSeed, Wallet}
import org.bitcoinj.wallet.listeners.{WalletCoinsReceivedEventListener, WalletCoinsSentEventListener}
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

object WalletProvider {

  def createNewWallet: Wallet = new Wallet(Network.params)

  def restoreWallet(seedData: DeterministicSeedData, sessionURI: String,
                    onreceived: (Wallet, Transaction, Coin, Coin) => Unit,
                    onsent: (Wallet, Transaction, Coin, Coin) => Unit,
                    onconfidencechanged: (Wallet, Transaction) => Unit): WalletAppKit = {

    val session = new URI(sessionURI).getHost

    SessionManager.getKitBySession(session) match {
      case Some(kit) => kit
      case None => {
        println(s"----------------------------------------> Wallet for session: $session")
        val kit: WalletAppKit = new WalletAppKit(Network.params, new File("."), s"restorewallet-${session}_${System.currentTimeMillis}")
        kit.restoreWalletFromSeed(new DeterministicSeed(seedData.seedcode, null, seedData.passphrase, seedData.creationtime))
        kit.setBlockingStartup(false)
        kit.startAsync
        kit.awaitRunning

        kit.wallet().allowSpendingUnconfirmedTransactions()
        if(Network.networkMode.equals("TEST")) kit.useTor()

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

        kit.setDownloadListener(new DownloadProgressTracker(){
          override def doneDownload() = {
            println(s"--------------------------------------> Download Complete for: $session")
            val content =
              ("sessionURI" -> sessionURI) ~
                ("address" -> s"${kit.wallet().currentReceiveAddress().toString}") ~
                ("tx" -> "") ~
                ("prevBalance" -> "0") ~
                ("newBalance" -> kit.wallet().getBalance(Wallet.BalanceType.ESTIMATED).toString)
            SessionManager.cometMessageByHost(sessionURI, compact(render(
              ("msgType" -> "balanceChanged") ~ ("content" -> content))))
          }
        })

        println("send money to: " + kit.wallet.currentReceiveAddress.toString)

        SessionManager.storeKitBySession(session, kit)
        kit
      }
    }
  }

  def getReceiveAddress(seedData: DeterministicSeedData): Address = {
    val seed = new DeterministicSeed(seedData.seedcode, null, seedData.passphrase, seedData.creationtime)
    Wallet.fromSeed(Network.params, seed).freshReceiveAddress()
  }
}
