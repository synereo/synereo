package com.biosimilarity.evaluator.spray.wallet

import org.json4s.JsonAST.{JField, JObject, JString}
import org.json4s.{native, _}

case class DeterministicSeedData(seedcode: String, passphrase: String, creationtime: Long)

object DeterministicSeedData {
  val hints = new ShortTypeHints(classOf[DeterministicSeedData] :: Nil) {
    override def serialize: PartialFunction[Any, JObject] = {
      case seed: DeterministicSeedData =>
        JObject(JField("seedcode", JString(seed.seedcode))
          :: JField("passphrase", JString(seed.passphrase))
          :: JField("creationtime", JInt(seed.creationtime))
          :: Nil)
    }
    override def deserialize: PartialFunction[(String, JObject), Any] = {
      case ("DeterministicSeedData",
        JObject(JField("seedcode", JString(sc))
          :: JField("passphrase", JString(pp))
          :: JField("creationtime", JInt(ct))
          :: Nil)) => DeterministicSeedData(sc, pp, ct.toLong)
    }
  }

  implicit val formats = native.Serialization.formats(hints)
}
