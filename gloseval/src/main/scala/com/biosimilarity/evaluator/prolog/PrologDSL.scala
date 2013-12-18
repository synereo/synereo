package com.biosimilarity.evaluator.prolog

import com.biosimilarity.lift.model.store._

object PrologDSL {
  import scala.util.matching.Regex
  
  class CCLMaker(s: Symbol) {
    def apply(ccls: CnxnCtxtLabel[String,String,String] with Factual*) = new CnxnCtxtBranch(s.name, ccls.toList)
  }

  implicit def symToCCLMaker(s: Symbol) = new CCLMaker(s)
  implicit def strToCCLeaf(s: String) = new CnxnCtxtLeaf[String,String,String](("^[A-Z_]".r findFirstIn s) match {
    case None => Left(s)
    case Some(_) => Right(s)
  })
  def Var(s: String) = new CnxnCtxtLeaf[String,String,String](Right(s))
  def Ground(s: String) = new CnxnCtxtLeaf[String,String,String](Left(s))
}

