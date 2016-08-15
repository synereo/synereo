package com.biosimilarity.evaluator

import org.json4s._

object Api {

  // helpers
  case class Connection(source: String, target: String, label: String)
  case class EvalSubscribeContent(cnxns: List[Connection], label: String, value: String, uid: String)
  case class EvalSubscribeExpression(msgType: String, content: EvalSubscribeContent)


  trait RequestContent
  case class Request(msgType: String, content: RequestContent)

  // actual API
  case class AddAliasLabelsRequest(sessionURI: String, alias: String, labels: List[String]) extends RequestContent
  case class CloseSessionRequest(sessionURI: String) extends RequestContent
  case class CreateUserRequest(email: String, password: String, jsonBlob: JObject) extends RequestContent
  case class EstablishConnectionRequest(sessionURI: String, aURI: String, bURI: String, label: String) extends RequestContent
  case class EvalSubscribeRequest(sessionURI: String, expression: EvalSubscribeExpression) extends RequestContent
  case class GetAgentRequest(email: String, password: String) extends RequestContent
  case class GetAmpWalletAddress(sessionURI: String) extends RequestContent
  case class GetConnectionProfiles(sessionURI: String) extends RequestContent
  case class InitializeSessionRequest(agentURI: String) extends RequestContent
  case class OmniGetBalance(sessionURI: String) extends RequestContent
  case class OmniTransfer(sessionURI: String, target: String, amount: BigDecimal ) extends RequestContent
  case class SessionPing(sessionURI: String) extends RequestContent
  case class SetAmpWalletAddress(sessionURI: String, address: String) extends RequestContent
  case class SpawnSessionRequest(sessionURI: String) extends RequestContent
  case class StartSessionRecording(sessionURI: String) extends RequestContent
  case class StopSessionRecording(sessionURI: String) extends RequestContent
  case class UpdateUserRequest(sessionURI: String, jsonBlob: JObject) extends RequestContent

  def toReq[T <: RequestContent](cont: T) : Request = {
    val nm = cont.getClass.getSimpleName()
    val tnm = Character.toLowerCase(nm.charAt(0)) + nm.substring(1)
    Api.Request(tnm, cont)
  }

  trait ResponseContent
  case class InitializeSessionResponse(sessionURI: String, listOfAliases: List[String], defaultAlias: String,
                                       listOfLabels: List[String], lastActiveLabel: String, jsonBlob: JObject)
    extends ResponseContent

}