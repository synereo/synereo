package com.biosimilarity.evaluator.importer.models

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization.write

abstract class LabelDesc {
  def toTermString(refs : String => LabelDesc) : String = {
    this match {
      case smpl : SimpleLabelDesc =>
        val typ =  smpl.`type`.getOrElse("string")
        if (typ == "string") "\"" + smpl.value + "\""
        else typ + "(\""  + smpl.value + "\")"
      case cmplx : ComplexLabelDesc =>
        val lbls: List[String] = cmplx.components.map( _.toTermString(refs) )
        cmplx.functor + lbls.mkString("(", ",", ")")
      case ref : LabelRef => refs(ref.label).toTermString(refs)
    }
  }

}

object LabelDesc {
  implicit val formats = DefaultFormats
  def extractFrom(jobj: JObject): LabelDesc = {
    jobj.extractOpt[LabelRef] match {
      case Some(lblref) => lblref
      case _ => jobj.extractOpt[SimpleLabelDesc] match {
        case Some(smpl) => smpl
        case None =>
          val comps : List[LabelDesc] = (jobj \ "components").extract[List[JObject]].map(extractFrom)
          val id = (jobj \ "id").extract[Option[String]]
          val functor = (jobj \ "functor").extract[String]
          ComplexLabelDesc(id,functor,comps)
      }
    }
  }
}

case class SimpleLabelDesc(
  id: Option[String],
  value: String,
  `type`: Option[String]
) extends LabelDesc {

  implicit val formats = DefaultFormats
  def toJson = write(this)  
}

object SimpleLabelDesc {
  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[SimpleLabelDesc]
}

case class ComplexLabelDesc(
                            id: Option[String],
                            functor: String,
                            components: List[LabelDesc]
                          ) extends LabelDesc {
  implicit val formats = DefaultFormats
  def toJson = write(this)
}

object ComplexLabelDesc {
  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[ComplexLabelDesc]
}

case class LabelRef(label: String) extends LabelDesc {
  implicit val formats = DefaultFormats

  def toJson = write(this)
}

object LabelRef {
  implicit val formats = DefaultFormats
  def fromJson(json: String) = parse(json).extract[LabelRef]
}


