// -*- mode: Scala;-*- 
// Filename:    CnxnPath.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:56:00 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import java.net.URI

trait CnxnPath[Namespace,Tag] {
  def path : Seq[Namespace]
  def trgt : Tag
}

class CnxnLiteralPath[Namespace,Tag](
  override val path : Seq[Namespace],
  override val trgt : Tag
) extends CnxnPath[Namespace,Tag] 

class CnxnVarPath[Namespace,Var,Tag](
  override val path : Seq[Either[Namespace,Var]],
  override val trgt : Either[Tag,Var]
) extends CnxnPath[Either[Namespace,Var],Either[Tag,Var]]

case class Predicate[Namespace,Var,Tag](
  p : Either[Namespace,(Var,CnxnLabel[Namespace,Tag]) => Boolean]
)

class CnxnPredPath[Namespace,Var,Tag](
  override val path : Seq[Predicate[Namespace,Var,Tag]],
  override val trgt : Predicate[Namespace,Var,Tag]
) extends CnxnPath[Predicate[Namespace,Var,Tag],Predicate[Namespace,Var,Tag]]


