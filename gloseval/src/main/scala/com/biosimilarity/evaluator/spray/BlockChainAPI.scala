// -*- mode: Scala;-*- 
// Filename:    BlockChainAPI.scala 
// Authors:     lgm                                                    
// Creation:    Thu Apr  3 10:41:35 2014 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.spray

trait BlockChainAPI {
  // ----------------------------------------------------------------------------------------------------------
  // BlockChain URI's
  // ----------------------------------------------------------------------------------------------------------
  trait BlockChainCall {
    def uri : String
  }
  case class CreateWallet( 
    pw : String,
    api_code : String,
    priv : Option[String],
    label : Option[String],
    email : Option[String]
  ) extends BlockChainCall {
    override val uri : String = "https://blockchain.info/api/v2/create_wallet"
    override def toString() : String = {
      val options : List[Option[String]] =
        List(
          for( p <- priv ) yield { "priv" + "=" + p },
          for( l <- label ) yield { "label" + "=" + l },
          for( em <- email ) yield { "email" + "=" + em }
        )
      val optQryStr : String = 
        ( "" /: options )( { ( acc, e ) => e match { case Some( s ) => acc + "&" + s; case None => acc } } )
      
      (
        uri
        + "?"
        + "password" + "=" + pw
        + "&"
        + "api_code" + "=" + api_code
        + optQryStr
      )
    }
  }
}
