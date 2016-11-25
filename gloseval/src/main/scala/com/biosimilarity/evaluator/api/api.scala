package com.biosimilarity.evaluator.api

import org.json4s._

// helpers
case class Connection(source: String, target: String, label: String)
case class EvalSubscribeContent(cnxns: List[Connection], label: String, value: Option[String], uid: Option[String])
case class EvalSubscribeExpression(msgType: String, content: EvalSubscribeContent)

// actual API
case class Request(msgType: String, content: RequestContent)

sealed trait RequestContent {
  def asRequest: Request = {
    val nm: String  = this.getClass.getSimpleName
    val tnm: String = Character.toLowerCase(nm.charAt(0)) + nm.substring(1)
    Request(tnm, this)
  }
}
case object VersionInfoRequest extends RequestContent {
  override def asRequest: Request = Request("versionInfoRequest", this)
}
case class ConfirmEmailToken(token: String)                                                          extends RequestContent
case class CreateUserRequest(email: String, password: String, jsonBlob: JObject)                     extends RequestContent
case class CreateUserStep1Request(email: String)                                                     extends RequestContent
case class CreateUserStep2Request(email: String, salt: String, verifier: String, jsonBlob: JObject)  extends RequestContent
case class GetAgentRequest(email: String, password: String)                                          extends RequestContent
case class GetConnectionProfiles(sessionURI: String)                                                 extends RequestContent
case class UpdateUserRequest(sessionURI: String, jsonBlob: JObject)                                  extends RequestContent
case class StartSessionRecording(sessionURI: String)                                                 extends RequestContent
case class StopSessionRecording(sessionURI: String)                                                  extends RequestContent
case class SpawnSessionRequest(sessionURI: String)                                                   extends RequestContent
case class SessionPing(sessionURI: String)                                                           extends RequestContent
case class InitializeSessionRequest(agentURI: String)                                                extends RequestContent
case class InitializeSessionStep1Request(agentURI: String)                                           extends RequestContent
case class InitializeSessionStep2Request(agentURI: String)                                           extends RequestContent
case class CloseSessionRequest(sessionURI: String)                                                   extends RequestContent
case class AddAliasLabelsRequest(sessionURI: String, alias: String, labels: List[String])            extends RequestContent
case class EstablishConnectionRequest(sessionURI: String, aURI: String, bURI: String, label: String) extends RequestContent
case class BeginIntroductionRequest(sessionURI: String, alias: String,
                                    aConnection: Connection, bConnection: Connection,
                                    aMessage: String, bMessage: String)                              extends RequestContent
case class EvalSubscribeRequest(sessionURI: String, expression: EvalSubscribeExpression)             extends RequestContent
case class ResetDatabaseRequest(sessionURI: String)                                                  extends RequestContent
case class GetAmpWalletAddress(sessionURI: String)                                                   extends RequestContent
case class SetAmpWalletAddress(sessionURI: String, address: String)                                  extends RequestContent
case class OmniTransfer(sessionURI: String, target: String, amount: BigDecimal)                      extends RequestContent
case class OmniGetBalance(sessionURI: String)                                                        extends RequestContent
case class OmniBalanceRequest(sessionURI: String)                                                    extends RequestContent
case class SendAmpsRequest(sessionURI: String, target: String, amount: String)                       extends RequestContent

sealed trait ResponseContent {

  implicit val formats = org.json4s.DefaultFormats

  protected def toJObject: JObject = Extraction.decompose(this).asInstanceOf[JObject]

  def asResponse: Response = {
    val nm: String  = this.getClass.getSimpleName
    val tnm: String = Character.toLowerCase(nm.charAt(0)) + nm.substring(1)
    Response(tnm, toJObject)
  }

}
case class VersionInfoResponse(glosevalVersion: String, scalaVersion: String, mongoDBVersion: String, rabbitMQVersion: String)
    extends ResponseContent {
  override def toString: String = s"""|GLoSEVal version: $glosevalVersion
                                      |Scala version: $scalaVersion
                                      |MongoDB version: $mongoDBVersion
                                      |RabbitMQ version: $rabbitMQVersion""".stripMargin
}
case class InitializeSessionStep1Response(s: String, B: String) extends ResponseContent
case class InitializeSessionResponse(sessionURI: String,
                                     defaultAlias: String,
                                     jsonBlob: JObject,
                                     lastActiveLabel: String,
                                     listOfAliases: List[String],
                                     listOfConnections: List[Connection],
                                     listOfLabels: List[String],
                                     bitcoinNetworkMode: String,
                                     M2: String)
    extends ResponseContent
case class CreateUserStep1Response(salt: String)                                                   extends ResponseContent
case class CreateUserStep2Response(agentURI: String)                                               extends ResponseContent
case class CreateUserWaiting(token: String)                                                        extends ResponseContent
case class ConnectionProfileResponse(sessionURI: String, connection: Connection, jsonBlob: JValue) extends ResponseContent
case class InitializeSessionError(reason: String)                                                  extends ResponseContent
case class ApiError(reason: String)                                                                extends ResponseContent
case class OmniBalanceResponse(sessionURI: String , amp: String, btc: String, address: String)     extends ResponseContent

case class Response(msgType: String, content: JObject) {

  implicit val formats = org.json4s.DefaultFormats

  def extractResponseContent: ResponseContent = msgType match {
    case "createUserError"                => content.extract[ApiError]
    case "createUserStep1Response"        => content.extract[CreateUserStep1Response]
    case "createUserStep2Response"        => content.extract[CreateUserStep2Response]
    case "createUserWaiting"              => content.extract[CreateUserWaiting]
    case "connectionProfileResponse"      => content.extract[ConnectionProfileResponse]
    case "initializeSessionError"         => content.extract[InitializeSessionError]
    case "initializeSessionResponse"      => content.extract[InitializeSessionResponse]
    case "initializeSessionStep1Response" => content.extract[InitializeSessionStep1Response]
    case "versionInfoResponse"            => content.extract[VersionInfoResponse]
    case "omniBalanceResponse"            => content.extract[OmniBalanceResponse]
  }
}
