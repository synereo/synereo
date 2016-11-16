package helpers.wiremock

import java.net.URI

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.core.WireMockConfiguration._
import com.synereo.wallet.config.RPCConfiguration

class OmniCoreStubServer extends RPCConfiguration with Serializable {

  val RPC_RESPONSE_GETINFO: String = "{\n  \"result\": {\n    \"omnicoreversion_int\": 1100100,\n    \"omnicoreversion\": \"0.0.11.1-rel\",\n    \"mastercoreversion\": \"0.0.11.1-rel\",\n    \"bitcoincoreversion\": \"0.10.4\",\n    \"commitinfo\": \"\",\n    \"block\": 173,\n    \"blocktime\": 1478297886,\n    \"blocktransactions\": 0,\n    \"totaltrades\": 0,\n    \"totaltransactions\": 38,\n    \"alerts\": []\n  },\n  \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_OMNI_GETBALANCE = "{\n  \"result\": {\n\"balance\" : \"42.00000000\",\n\"reserved\" : \"0.00000000\"\n},\n \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_IMPORTADDRESS = "{\n  \"result\": null,\n \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_LISTUNSPENT = "{\n  \"result\": [\n{\n\"txid\" : \"a45e45c43c0fa92016b9d6ee3d403e67e2803fab60ee6d009fe9bb1bfb262ac0\",\n\"vout\" : 0,\n\"address\" : \"mrgqGm3pU8Au7wrRys34XUFvk5eKTa991j\",\n\"account\" : \"\",\n\"scriptPubKey\" : \"76a9147a87ae6a91fbd6ef2bff8c1806ab1137783f824388ac\",\n\"amount\" : 10.00000000,\n\"confirmations\" : 2,\n\"spendable\" : true\n},\n{\n\"txid\" : \"a4f0605914d05abdf2bafcf48624f97e75942741f2df7533c610363de6201d5d\",\n\"vout\" : 1,\n\"address\" : \"mrgqGm3pU8Au7wrRys34XUFvk5eKTa991j\",\n\"account\" : \"\",\n\"scriptPubKey\" : \"76a9147a87ae6a91fbd6ef2bff8c1806ab1137783f824388ac\",\n\"amount\" : 0.00002730,\n\"confirmations\" : 1,\n\"spendable\" : true\n}\n],\n \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_ERROR = "{\n  \"result\": null,\n  \"error\": {\n    \"code\": -1,\n    \"message\": method not supported\"\"\n  },\n  \"id\": null\n}"
  val RPC_RESPONSE_SETGENERATE = "{\n  \"result\": [\"4b99b67c94162a651c468e4f77e75bee385eab65c184cc2af558cda7861064dd\"],\n \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_GETTRANSACTION = "{\n  \"result\": {\n\"amount\" : 0.00000000,\n\"fee\" : -0.00003352,\n\"confirmations\" : 2,\n\"blockhash\" : \"2636b3a7f4d6679bf00cd035f1fd3ef325bf99bf07048f05cfe3e0a56ebf1907\",\n\"blockindex\" : 1,\n\"blocktime\" : 1479298111,\n\"txid\" : \"a45e45c43c0fa92016b9d6ee3d403e67e2803fab60ee6d009fe9bb1bfb262ac0\",\n\"walletconflicts\" : [\n],\n\"time\" : 1479298097,\n\"timereceived\" : 1479298097,\n\"details\" : [\n{\n\"account\" : \"\",\n\"address\" : \"mrgqGm3pU8Au7wrRys34XUFvk5eKTa991j\",\n\"category\" : \"send\",\n\"amount\" : -10.00000000,\n\"vout\" : 0,\n\"fee\" : -0.00003352\n},\n{\n\"account\" : \"\",\n\"address\" : \"mrgqGm3pU8Au7wrRys34XUFvk5eKTa991j\",\n\"category\" : \"receive\",\n\"amount\" : 10.00000000,\n\"vout\" : 0\n}\n],\n\"hex\" : \"01000000044073e4dcd411e0db578bc06748c7f998be485b587907e3aec65d345cb4706506010000006b483045022100e9254ec509815540f5df813f3efe2ab0cba1cc83223db0b58cea8fef6cb9363502202bbf12e85732542cbd03fb2a5fc43247e7d0a71560089ba1b6335618496c6c2d01210388dc8d3516f76795262a50f5c5cbbf083dbf243959fcbd891b3ad9ff9e4d5c9affffffff8955f12b78399c0a478ba511d5048750809a6af4b84f508f0b27356f1b99ea22010000006b483045022100d114dccbfc403970ab0f14142592780305f22b5077b09a99f9c4dcfedfe5245602204ae3f2ee5a3cb421b06a231465075da66300c811b7a5e8950b5f1f67e780fdcb0121021ab4efecd16d11c1aa36b5c7e627b06976327f72f038c9dc908168ef2470ab73ffffffffdf5257a1b662cf34c0ee1330777fb9996deb48ea6fd0df9ebc1f4c11b92c612e010000006b483045022100bbb4768112d9fc63db6bafbcb4d2d430fc710c1cc044cbb6e4c51993c652ef09022037004e982e24638f2d7f6625423a22dfa873f0077afbc85b6ac553445c4b87e701210339a94cc5700b7b57777b909f0fda8967118e82efc388ed42fc06da7eff48ce64fffffffff4a530b84b866497c7344760724e12600a5fee9670c2370c6c98f4bc159f201a010000006b483045022100b73e6e8cc7fc903f7886eaf72751ac50ee5a1f913cc3f1b95a9134d241eb41c302202be93c387cb7fc22bc0a3ee2c61708c13f440cfaecc8943a6d22ea980a347f44012102613834dfa8a3a9104ccd87006b9c8b04359348470546b5ecd2df4048c2c794d2ffffffff0200ca9a3b000000001976a9147a87ae6a91fbd6ef2bff8c1806ab1137783f824388ac8e367207000000001976a9148bd694cc540470904d36d5a00f4a79c93548de3b88ac00000000\"\n},\n \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_SENDRAWTRANSACTION = "{\n  \"result\": \"4b99b67c94162a651c468e4f77e75bee385eab65c184cc2af558cda7861064dd\",\n \"error\": null,\n  \"id\": null\n}"

  val uri = new URI(rpcURL)
  val port = uri.getPort
  val host = uri.getHost

  val wireMockServer = new WireMockServer(wireMockConfig().port(port))

  def start() = {
    wireMockServer.start()

    WireMock.configureFor(host, port)

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'getinfo')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_GETINFO)))

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'importaddress')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_IMPORTADDRESS)))

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'getbalance_MP')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_OMNI_GETBALANCE)))

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'listunspent')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_LISTUNSPENT)))

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'setgenerate')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_SETGENERATE)))

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'getrawtransaction')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_GETTRANSACTION)))

    WireMock.stubFor(post(anyUrl())
      .withBasicAuth(rpcUser, rpcPassword)
      .withRequestBody(matchingJsonPath("$[?(@.method == 'sendrawtransaction')]"))
      .willReturn(aResponse().withBody(RPC_RESPONSE_SENDRAWTRANSACTION)))

    println(Console.GREEN + "Omni Core Stub server started" + Console.RESET)
  }

  def stop() = {
    wireMockServer.stop()
    println(Console.GREEN + "Omni Core Stub server stopped" + Console.RESET)
  }

}
