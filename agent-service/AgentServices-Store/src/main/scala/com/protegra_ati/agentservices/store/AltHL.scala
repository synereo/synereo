// -*- mode: Scala;-*- 
// Filename:    HL.scala 
// Authors:     lgm                                                    
// Creation:    Wed Feb 27 11:45:05 2013 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution.portable.dsl

import com.biosimilarity.evaluator.distribution.PortableAgentCnxn
import com.biosimilarity.lift.model.store._
import com.protegra_ati.agentservices.store._

import java.util.UUID
import java.net.URI

trait HLExpr
trait Query {
  def label : CnxnCtxtLabel[String,String,String]
  def cnxns : Seq[PortableAgentCnxn]
}
trait ValueExpr[Value] {
  def value : Value
}
trait Modification[Value] extends ValueExpr[Value]
trait Result[Value] extends ValueExpr[Value]

case object Bottom extends HLExpr

case class FeedExpr(
  override val label : CnxnCtxtLabel[String,String,String],
  override val cnxns : Seq[PortableAgentCnxn]
) extends HLExpr with Query

case class ReadExpr(
  override val label : CnxnCtxtLabel[String,String,String],
  override val cnxns : Seq[PortableAgentCnxn]
) extends HLExpr with Query

case class FetchExpr(
  override val label : CnxnCtxtLabel[String,String,String],
  override val cnxns : Seq[PortableAgentCnxn]
) extends HLExpr with Query

case class ScoreExpr(
  override val label : CnxnCtxtLabel[String,String,String],
  override val cnxns : Seq[PortableAgentCnxn],
  staff : Either[Seq[PortableAgentCnxn],Seq[CnxnCtxtLabel[String,String,String]]]
) extends HLExpr with Query

case class InsertContent[Value](
  override val label : CnxnCtxtLabel[String,String,String],
  override val cnxns : Seq[PortableAgentCnxn],
  override val value : Value
) extends HLExpr with Query with Modification[Value]

case class ResultExpr[Value](
  substitution : Option[List[(String,CnxnCtxtLabel[String,String,String])]],
  matchedLabel : CnxnCtxtLabel[String,String,String],
  override val value : Value
) extends HLExpr with Result[Value]
case class PostedExpr[Value](
  override val value : Value
) extends HLExpr with Result[Value]

object PortableToInternalDSL {
  implicit def toInternalFormat(
    expr : FeedExpr
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.FeedExpr = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.FeedExpr( 
      expr.label,
      expr.cnxns
    )
  }
  implicit def toInternalFormat(
    expr : ReadExpr
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.ReadExpr = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.ReadExpr( 
      expr.label,
      expr.cnxns
    )
  }
  implicit def toInternalFormat(
    expr : FetchExpr
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.FetchExpr = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.FetchExpr( 
      expr.label,
      expr.cnxns
    )
  }
  implicit def toInternalFormat(
    expr : ScoreExpr
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.ScoreExpr = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.ScoreExpr( 
      expr.label,
      expr.cnxns,
      expr.staff
    )
  }
  implicit def toInternalFormat[Value](
    expr : InsertContent[Value]
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.InsertContent[Value] = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.InsertContent( 
      expr.label,
      expr.cnxns,
      expr.value
    )
  }
  implicit def toInternalFormat[Value](
    expr : ResultExpr[Value]
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.ResultExpr[Value] = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.ResultExpr( 
      expr.substitution,
      expr.matchedLabel,
      expr.value
    )
  }
  implicit def toInternalFormat[Value](
    expr : PostedExpr[Value]
  ) : com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr[Value] = {
    com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr( 
      expr.value
    )
  }
}

object InternalToPortableDSL {
  implicit def toPortableFormat(
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.FeedExpr
  ) : FeedExpr  = {
    FeedExpr( 
      expr.label,
      expr.cnxns
    )
  }
  implicit def toPortableFormat(
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.ReadExpr
  ) : ReadExpr  = {
    ReadExpr( 
      expr.label,
      expr.cnxns
    )
  }
  implicit def toPortableFormat(
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.FetchExpr
  ) : FetchExpr  = {
    FetchExpr( 
      expr.label,
      expr.cnxns
    )
  }
  implicit def toPortableFormat(
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.ScoreExpr
  ) : ScoreExpr  = {
    ScoreExpr( 
      expr.label,
      expr.cnxns,
      expr.staff
    )
  }
  implicit def toPortableFormat[Value](
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.InsertContent[Value]
  ) : InsertContent[Value] = {
    InsertContent( 
      expr.label,
      expr.cnxns,
      expr.value
    )
  } 
  implicit def toPortableFormat[Value](
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.ResultExpr[Value]
  ) : ResultExpr[Value] = {
    ResultExpr( 
      expr.substitution,
      expr.matchedLabel,
      expr.value
    )
  }
  implicit def toPortableFormat[Value](
    expr : com.biosimilarity.evaluator.distribution.ConcreteHL.PostedExpr[Value]
  ) : PostedExpr[Value] = {
    PostedExpr( 
      expr.value
    )
  }
}
