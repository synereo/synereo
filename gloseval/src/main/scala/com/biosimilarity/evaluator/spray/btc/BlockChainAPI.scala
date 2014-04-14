// -*- mode: Scala;-*- 
// Filename:    BlockChainAPI.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr  3 10:41:35 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray
import spray.json._
import DefaultJsonProtocol._

trait BlockChainAPIT {  
  val createWalletAPICode = "6ec81336-5934-44a0-92ed-2a19aa379320"
  val createWalletURIStr : String = "https://blockchain.info/api/v2/create_wallet"
  // ----------------------------------------------------------------------------------------------------------
  // BlockChain URI's
  // ----------------------------------------------------------------------------------------------------------
  trait BlockChainData 
  trait BlockChainCall[Data <: BlockChainData] {
    def data : Data
    def url : java.net.URL
    override def toString() : String = url + "?" + data.toString
  }  

  case class CreateWalletData( 
    password : String,
    api_code : String,
    label : String,
    email : String
  ) extends BlockChainData {
    override def toString() : String = {            
      (
        "password" + "=" + password
        + "&"
        + "api_code" + "=" + api_code
        + "&"
        + "label" + "=" + label
        + "&"
        + "email" + "=" + email
      )
    }
  }

  case class CreateReceivingAddressData( 
    address : String,
    callback : String,
    method : String = "create"
  ) extends BlockChainData {
    override def toString() : String = {            
      (
        "method" + "=" + "create"
        + "&"
        + "address" + "=" + address
        + "&"
        + "callback" + "=" + callback
      )
    }
  }

  case class MakeOutgoingPaymentData( 
    main_password : String,
    to : String,
    amount : Double,
    from : String,
    note : String
  ) extends BlockChainData {
    override def toString() : String = {            
      (
        "main_password" + "=" + main_password
        + "&"
        + "to" + "=" + to
        + "&"
        + "amount" + "=" + amount
        + "&"
        + "from" + "=" + from
        + "&"
        + "note" + "=" + note
      )
    }
  }

  case class CreateWalletResponse(
    guid : String,
    address : String,
    link : String
  ) extends BlockChainData

  case class ReceivingAddressResponse(
    fee_percent : Double,
    destination : String,
    input_address : String,
    callback_url : String
  ) extends BlockChainData
  
  private def getCCParams(cc: AnyRef) =
    (Map[String, String]() /: cc.getClass.getDeclaredFields)(
      {(a, f) => f.setAccessible(true); a + (f.getName -> f.get(cc).toString) }
    )
  def toMap( cwd : BlockChainData ) : Map[String,String] = getCCParams( cwd )

  implicit val simpleCreateWalletFormat : RootJsonFormat[CreateWalletData] = jsonFormat4( CreateWalletData )

  case class CreateWallet( 
    override val data : CreateWalletData,
    override val url : java.net.URL = new java.net.URL( "https://blockchain.info/api/v2/create_wallet" )
  ) extends BlockChainCall[CreateWalletData] {    
  }

  case class CreateReceivingAddress( 
    override val data : CreateReceivingAddressData,
    override val url : java.net.URL = new java.net.URL( "https://blockchain.info/api/receive" )
  ) extends BlockChainCall[CreateReceivingAddressData] {    
  }

  case class MakeOutgoingPayment( 
    override val data : MakeOutgoingPaymentData,
    guid : String    
  ) extends BlockChainCall[MakeOutgoingPaymentData] {    
    override def url : java.net.URL = new java.net.URL( s"""https://blockchain.info/merchant/${guid}/payment""" )
  }
}

object BlockChainAPI extends BlockChainAPIT with Serializable

