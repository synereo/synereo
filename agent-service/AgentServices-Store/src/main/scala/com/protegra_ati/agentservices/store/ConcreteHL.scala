// -*- mode: Scala;-*-
// Filename:    ConcreteHL.scala
// Authors:     lgm
// Creation:    Sat Apr 27 02:42:41 2013
// Copyright:   Not supplied
// Description:
// ------------------------------------------------------------------------

package com.biosimilarity.evaluator.distribution

import java.net.URI

import com.biosimilarity.lift.model.store._

case class PortableAgentCnxn(src: URI, label: String, trgt: URI)
    extends Cnxn[URI, String, URI]

case class PortableAgentBiCnxn(readCnxn: PortableAgentCnxn,
                               writeCnxn: PortableAgentCnxn)

object ConcreteHL extends Serializable {

  type Label = CnxnCtxtLabel[String, String, String]
  type Substitution = Option[List[(String, Label)]]

  type Cnxn = PortableAgentCnxn
  type BiCnxn = PortableAgentBiCnxn

  trait HLExpr

  trait Query {
    def label: Label
    def cnxns: Seq[Cnxn]
  }

  trait ValueExpr[Value] {
    def value: Value
  }

  trait Modification[Value] extends ValueExpr[Value]
  trait Result[Value] extends ValueExpr[Value]

  case object Bottom extends HLExpr

  case class FlatKeyBouncer(label: Label) extends HLExpr

  case class FeedExpr(override val label: Label, override val cnxns: Seq[Cnxn])
      extends HLExpr
      with Query

  case class ReadExpr(override val label: Label, override val cnxns: Seq[Cnxn])
      extends HLExpr
      with Query

  case class FetchExpr(override val label: Label,
                       override val cnxns: Seq[Cnxn])
      extends HLExpr
      with Query

  case class GetExpr(override val label: Label, override val cnxns: Seq[Cnxn])
      extends HLExpr
      with Query

  case class ScoreExpr(override val label: Label,
                       override val cnxns: Seq[Cnxn],
                       staff: Either[Seq[Cnxn], Seq[Label]])
      extends HLExpr
      with Query

  case class CancelExpr(override val label: Label,
                        override val cnxns: Seq[Cnxn])
      extends HLExpr
      with Query

  case class InsertContent[Value](override val label: Label,
                                  override val cnxns: Seq[Cnxn],
                                  override val value: Value)
      extends HLExpr
      with Query
      with Modification[Value]

  case class InsertContentV[Value](override val label: Label,
                                   override val cnxns: Seq[Cnxn],
                                   override val value: Value)
      extends HLExpr
      with Query
      with Modification[Value]

  case class PutContent[Value](override val label: Label,
                               override val cnxns: Seq[Cnxn],
                               override val value: Value)
      extends HLExpr
      with Query
      with Modification[Value]

  case class ResultExpr[Value](substitution: Substitution,
                               matchedLabel: Label,
                               override val value: Value)
      extends HLExpr
      with Result[Value]

  case class PostedExpr[Value](override val value: Value)
      extends HLExpr
      with Result[Value]

  // If you have got this far, and know this does, please contact us
  // for possible employment
  trait SystemRequest
  trait SystemResponse

  case class RunProcessRequest(command: String,
                               workingDir: Option[String],
                               environmentVars: Seq[(String, String)])
      extends HLExpr
      with SystemRequest

  case class RunProcessResponse(exitCode: Int,
                                stdOut: Seq[String],
                                stdErr: Seq[String])
      extends HLExpr
      with SystemResponse

  // Groundwork for administration of nodes
  trait AdminRequest
  trait AdminResponse

  // Dynamic connection of clients
  case class StartEngineRequest(
    configFileName : String
  ) extends HLExpr
      with AdminRequest
  case class StartEngineResponse(
    configFileName : String
  ) extends HLExpr
      with AdminRequest

  case class ConnectToClientRequest(
    uris : Seq[URI]
  ) extends HLExpr
      with AdminRequest

  case class ConnectToClientResponse(
    biCnxn : PortableAgentBiCnxn
  ) extends HLExpr
  with AdminResponse
}
