// -*- mode: Scala;-*- 
// Filename:    PathStore.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:34:16 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.store

import java.net.URI

trait CnxnLabel[Namespace,Tag]
class CnxnLeaf[Namespace,Tag]( val tag : Tag )
extends CnxnLabel[Namespace,Tag]

object CnxnLeaf {
  def unapply[Namespace,Tag](
    cnxnLeaf : CnxnLeaf[Namespace,Tag]
  ) : Option[( Tag )] = {
    Some( ( cnxnLeaf.tag ) )
  }
}

class CnxnBranch[Namespace,Tag](
  val nameSpace : Namespace,
  val labels : List[CnxnLabel[Namespace,Tag]]
) extends CnxnLabel[Namespace,Tag]

object CnxnBranch {
  def unapply[Namespace,Tag](
    cnxnBranch : CnxnBranch[Namespace,Tag]
  ) : Option[( Namespace, List[CnxnLabel[Namespace,Tag]] )] = {
    Some( ( cnxnBranch.nameSpace, cnxnBranch.labels ) )
  }
}

trait CnxnCtxtLabel[Namespace,Var,Tag]
extends CnxnLabel[Either[Namespace,Var],Either[Tag,Var]] 

trait CnxnCtxtInjector[Namespace,Var,Tag] {
  def injectLabel( cLabel : CnxnLabel[Namespace,Tag] )
  : CnxnCtxtLabel[Namespace,Var,Tag] = {
    cLabel match {
      case cLeaf : CnxnLeaf[Namespace,Tag] =>
	inject( cLeaf )
      case cBranch : CnxnBranch[Namespace,Tag] =>
	inject( cBranch )
    }
  }
  def inject( cLabel : CnxnLeaf[Namespace,Tag] )
  : CnxnCtxtLabel[Namespace,Var,Tag] = {
    new CnxnCtxtLeaf( Left( cLabel.tag ) )
  }
  def inject( cLabel : CnxnBranch[Namespace,Tag] )
  : CnxnCtxtLabel[Namespace,Var,Tag] = {
    new CnxnCtxtBranch(
      cLabel.nameSpace,
      cLabel.labels.map( injectLabel( _ ) )
    )
  }
}

class CnxnCtxtLeaf[Namespace,Var,Tag]( val tag : Either[Tag,Var] )
extends CnxnCtxtLabel[Namespace,Var,Tag] 

object CnxnCtxtLeaf {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtLeaf : CnxnCtxtLeaf[Namespace,Var,Tag]
  ) : Option[( Either[Tag,Var] )] = {
    Some( ( cnxnCtxtLeaf.tag ) )
  }
}

class CnxnCtxtBranch[Namespace,Var,Tag](
  val nameSpace : Namespace,
  val labels : List[CnxnCtxtLabel[Namespace,Var,Tag]]
) extends CnxnCtxtLabel[Namespace,Var,Tag]

object CnxnCtxtBranch {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtBranch : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : Option[( Namespace, List[CnxnCtxtLabel[Namespace,Var,Tag]] )] = {
    Some( ( cnxnCtxtBranch.nameSpace, cnxnCtxtBranch.labels ) )
  }
}

trait Cnxn[Src,Label,Trgt] {
  def src   : Src
  def label : Label
  def trgt  : Trgt
}
class CCnxn[Src,Label,Trgt](
  override val src : Src,
  override val label : Label,
  override val trgt : Trgt
) extends Cnxn[Src,Label,Trgt]

object CCnxn {
  def unapply[Src,Label,Trgt](
    cnxn : CCnxn[Src,Label,Trgt]
  ) : Option[(Src,Label,Trgt)] = {
    Some( ( cnxn.src, cnxn.label, cnxn.trgt ) )
  }  
}

case class StringCnxnLeaf( override val tag : String )
     extends CnxnLeaf[String,String]( tag )

case class StringCnxnBranch(
  override val nameSpace : String,
  override val labels : List[CnxnLabel[String,String]]
) extends CnxnBranch[String,String]( nameSpace, labels )

