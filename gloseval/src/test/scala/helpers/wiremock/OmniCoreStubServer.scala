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
  val RPC_RESPONSE_LISTUNSPENT = "{\n  \"result\": [],\n \"error\": null,\n  \"id\": null\n}"
  val RPC_RESPONSE_ERROR = "{\n  \"result\": null,\n  \"error\": {\n    \"code\": -1,\n    \"message\": method not supported\"\"\n  },\n  \"id\": null\n}"
  val RPC_RESPONSE_SENDGENERATE = "{\n  \"result\": [\"4b99b67c94162a651c468e4f77e75bee385eab65c184cc2af558cda7861064dd\"],\n \"error\": null,\n  \"id\": null\n}"

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
      .willReturn(aResponse().withBody(RPC_RESPONSE_LISTUNSPENT)))

    println(Console.GREEN + "Omni Core Stub server started" + Console.RESET)
  }

  def stop() = {
    wireMockServer.stop()
    println(Console.GREEN + "Omni Core Stub server stopped" + Console.RESET)
  }

}
