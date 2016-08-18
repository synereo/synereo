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
  case class CreateUserRequest(email: String,password: String,jsonBlob: JObject) extends RequestContent
  case class CreateUserStep1Request(email: String) extends RequestContent
  case class CreateUserStep2Request(email: String, salt: String, verifier: String, jsonBlob: JObject) extends RequestContent
  case class GetAgentRequest(email: String,password: String) extends RequestContent
  case class GetConnectionProfiles(sessionURI: String) extends RequestContent
  case class UpdateUserRequest(sessionURI: String,jsonBlob: JObject) extends RequestContent
  case class StartSessionRecording(sessionURI: String) extends RequestContent
  case class StopSessionRecording(sessionURI: String) extends RequestContent
  case class SpawnSessionRequest(sessionURI: String) extends RequestContent
  case class SessionPing(sessionURI: String) extends RequestContent
  case class InitializeSessionRequest(agentURI: String) extends RequestContent
  case class InitializeSessionStep1Request(agentURI: String) extends RequestContent
  case class InitializeSessionStep2Request(agentURI: String) extends RequestContent
  case class CloseSessionRequest(agentURI: String) extends RequestContent
  case class AddAliasLabelsRequest(sessionURI: String, alias: String, labels: List[String]) extends RequestContent
  case class EstablishConnectionRequest(sessionURI: String, aURI: String, bURI: String, label: String) extends RequestContent
  case class EvalSubscribeRequest(sessionURI: String, expression: EvalSubscribeExpression) extends RequestContent
  case class ResetDatabaseRequest(sessionURI: String) extends RequestContent
  case class GetAmpWalletAddress(sessionURI: String) extends RequestContent
  case class SetAmpWalletAddress(sessionURI: String, address: String) extends RequestContent
  case class OmniTransfer(sessionURI: String, target: String, amount: BigDecimal ) extends RequestContent
  case class OmniGetBalance(sessionURI: String) extends RequestContent

  def toReq[T <: RequestContent](cont: T) : Request = {
    val nm = cont.getClass.getSimpleName()
    val tnm = Character.toLowerCase(nm.charAt(0)) + nm.substring(1)
    Api.Request(tnm, cont)
  }


  sealed trait ResponseContent
  case class InitializeSessionStep1Response(salt: String, B: String) extends ResponseContent
  case class InitializeSessionResponse(sessionURI: String, M2: String) extends ResponseContent
  case class CreateUserStep1Response(salt: String) extends ResponseContent
  case class CreateUserStep2Response(agentURI: String) extends ResponseContent
  case class ApiError(reason: String) extends ResponseContent

  sealed trait Response
  case class ApiResponse(msgType: String, content: JObject) extends Response {
    val responseContent = (msgType, content) match {
      case ("createUserStep1Response", JObject(JField("salt", JString(s)) :: Nil)) => CreateUserStep1Response(s)
      case ("createUserStep2Response", JObject(JField("agentURI", JString(au)) :: Nil)) => CreateUserStep2Response(au)
      case ("initializeSessionStep1Response", JObject(JField("s", JString(s)) :: JField("B", JString(b)) :: Nil)) =>
        InitializeSessionStep1Response(s, b)
      case ("initializeSessionResponse", JObject(JField("sessionURI", JString(ssn)) ::
                                                 JField("listOfAliases", JArray(la)) ::
                                                 JField("defaultAlias", JString(da)) ::
                                                 JField("listOfLabels", JArray(ll)) ::
                                                 JField("listOfConnections", JArray(lc)) ::
                                                 JField("lastActiveLabel", JString(lal)) ::
                                                 JField("jsonBlob", JObject(jb)) ::
                                                 JField("M2", JString(m2)) :: Nil)) =>
        InitializeSessionResponse(ssn, m2)
      case ("createUserError", JObject(JField("reason", JString(r)) :: Nil)) => ApiError(r)
      case ("initializeSessionError", JObject(JField("reason", JString(r)) :: Nil)) => ApiError(r)
    }
  }

  val hints = new ShortTypeHints(classOf[ApiResponse] :: Nil) {
    override def serialize: PartialFunction[Any, JObject] = {
      case ar: ApiResponse =>
        JObject(JField("msgType", JString(ar.msgType)) :: JField("content", ar.content) :: Nil)
    }
    override def deserialize: PartialFunction[(String, JObject), Any] = {
      case ("ApiResponse", JObject(JField("msgType", JString(m)) :: JField(content, JObject(c)) :: Nil)) =>
        ApiResponse(m, JObject(c))
    }
  }

  implicit val formats = native.Serialization.formats(hints)

}