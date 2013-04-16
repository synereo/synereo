// -*- mode: Scala;-*- 
// Filename:    PathStore.scala 
// Authors:     lgm                                                    
// Creation:    Wed Aug 25 16:34:16 2010 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

import com.biosimilarity.lift.model.ApplicationDefaults
import com.biosimilarity.lift.lib.zipper._
import scala.collection.SeqProxy
import java.net.URI

trait CnxnLabel[Namespace, /*+*/Tag]
extends Tree[Tag] with SeqProxy[Either[Tag,CnxnLabel[Namespace,Tag]]]
  with Serializable {
  def up /* [Tag1 >: Tag] */ ( tOrC : Either[Tag,CnxnLabel[Namespace,Tag]] )
   : List[Tag] = {
    tOrC match {
      case Left( t ) => List( t )
      case Right( CnxnBranch( ns, lbls ) ) => {
	( List[Tag]() /: lbls.flatMap( _.self ) )(
	  {
	    ( acc, e ) => {
	      acc ++ up/*[Tag1]*/( e )
	    }
	  }
	)
      }
    }
  }

  def atoms : Seq[Tag] = { this flatMap( up ) }
}

trait OntologicalStatus
trait Factual extends OntologicalStatus
trait Hypothetical extends OntologicalStatus
trait Theoretical extends OntologicalStatus

class CnxnLeaf[Namespace,Tag]( val tag : Tag )
extends TreeItem[Tag]( tag )
with CnxnLabel[Namespace,Tag]
with Factual {
  override def self = List( Left( tag ) )
}

object CnxnLeaf {
  def unapply[Namespace,Tag](
    cnxnLeaf : CnxnLeaf[Namespace,Tag]
  ) : Option[( Tag )] = {
    Some( ( cnxnLeaf.tag ) )
  }
}

// case class CCnxnLeaf[Namespace,Tag]( val tag : Tag )
// extends CTreeItem[Tag]( tag )
// with CnxnLabel[Namespace,Tag]
// with Factual {
//   override def self = List( Left( tag ) )
// }

trait AbstractCnxnBranch[Namespace,Tag]
extends CnxnLabel[Namespace,Tag] {
  def nameSpace : Namespace
  def labels : List[CnxnLabel[Namespace,Tag]]
  override def self = labels.map( Right( _ ) )
}

class CnxnBranch[Namespace,Tag](
  override val nameSpace : Namespace,
  val factuals : List[CnxnLabel[Namespace,Tag] with Factual]
) extends TreeSection[Tag]( factuals )
with AbstractCnxnBranch[Namespace,Tag]
with CnxnLabel[Namespace,Tag]
with Factual {
  override def labels : List[CnxnLabel[Namespace,Tag]] = {
    factuals
  }
}

object CnxnBranch {
  def unapply[Namespace,Tag](
    cnxnBranch : CnxnBranch[Namespace,Tag]
  ) : Option[( Namespace, List[CnxnLabel[Namespace,Tag]] )] = {
    Some( ( cnxnBranch.nameSpace, cnxnBranch.labels ) )
  }
}

// case class CCnxnBranch[Namespace,Tag](
//   val nameSpace : Namespace,
//   val factuals : List[CnxnLabel[Namespace,Tag] with Factual]
// ) extends CTreeSection[Tag]( factuals )
// with AbstractCnxnBranch[Namespace,Tag]
// with CnxnLabel[Namespace,Tag]
// with Factual {
//   override def labels : List[CnxnLabel[Namespace,Tag]] = {
//     factuals
//   }
// }

trait CnxnCtxtLabel[Namespace,Var,/*+*/Tag]
extends CnxnLabel[Either[Namespace,Var],Either[Tag,Var]] {
  type U/*[Tag1]*/ =
    Either[
      Either[Tag,Var],
      CnxnLabel[Either[Namespace,Var],Either[Tag,Var]]
    ]
  override def up /*[Tag1 >: Tag]*/ ( tOrC : U/*[Tag1]*/ )
   : List[Either[Tag,Var]] = {
    tOrC match {
      case Left( t ) => List( t )
      case Right( CnxnCtxtLeaf( tOrV ) ) => List( tOrV )
      case Right( CnxnCtxtBranch( ns, lbls ) ) => {
	val selves : List[U/*[Tag1]*/] = lbls.flatMap( _.self )
	( List[Either[Tag,Var]]() /: selves )(
	  {
	    ( acc, e ) => {
	      acc ++ up/*[Tag1]*/( e )
	    }
	  }
	)
      }
    }
  }  

  def names : Seq[Either[Tag,Var]] = {
    atoms filter(
      {
	( ctxtLbl ) => {
	  ctxtLbl match { 
	    case Left( _ ) => false
	    case Right( _ ) => true
	  }
	}
      }
    )
  }
}

class CnxnCtxtLeaf[Namespace,Var,Tag]( val tag : Either[Tag,Var] )
extends TreeItem[Either[Tag,Var]]( tag )
with CnxnCtxtLabel[Namespace,Var,Tag]
with Factual {
  def this() = { this( null.asInstanceOf[Either[Tag,Var]] ) }
  override def self = List( Left( tag ) )
  override def toString = {
    tag match {
      //case Left( t ) => "<" + t + ">"
      case Left( t ) => "" + t + ""
      case Right( v ) => "'" + v
    }
  }
}

object CnxnCtxtLeaf {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtLeaf : CnxnCtxtLeaf[Namespace,Var,Tag]
  ) : Option[( Either[Tag,Var] )] = {
    Some( ( cnxnCtxtLeaf.tag ) )
  }
}

// case class CCnxnCtxtLeaf[Namespace,Var,Tag]( val tag : Either[Tag,Var] )
// extends CTreeItem[Either[Tag,Var]]( tag )
// with CnxnCtxtLabel[Namespace,Var,Tag]
// with Factual {
//   override def self = List( Left( tag ) )
//   override def toString = {
//     tag match {
//       case Left( t ) => "<" + t + ">"
//       case Right( v ) => "'" + v
//     }
//   }
// }

trait AbstractCnxnCtxtBranch[Namespace,Var,Tag]
extends CnxnCtxtLabel[Namespace,Var,Tag] {  
  override def self = labels.map( Right( _ ) )
  def nameSpace : Namespace
  def labels : List[CnxnCtxtLabel[Namespace,Var,Tag]]
  override def toString = {
    val lblStr =
      labels match {
	case albl :: rlbls => {
	  ( albl.toString /: rlbls )( 
	    {
	      ( acc, lbl ) => {
		acc + ", " + lbl
	      }
	    }
	  )
	}
	case Nil => ""
      }
    nameSpace + "(" + lblStr + ")"
  }
}

class CnxnCtxtBranch[Namespace,Var,Tag](
  override val nameSpace : Namespace,
  val factuals : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
) extends TreeSection[Either[Tag,Var]]( factuals )
with AbstractCnxnCtxtBranch[Namespace,Var,Tag]
with Factual {
  def this() = { this( null.asInstanceOf[Namespace], Nil ) }
  override def labels : List[CnxnCtxtLabel[Namespace,Var,Tag]] = {
    factuals
  }
  override def equals( o : Any ) : Boolean = {
    o match {
      case that : CnxnCtxtBranch[Namespace,Var,Tag] => {
	(
	  nameSpace.equals( that.nameSpace )
	  && factuals.equals( that.factuals ) 
	)
      }
      case _ => false
    }
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * nameSpace.hashCode )
      + ( 37 * factuals.hashCode )
    )
  }
}

// case class CCnxnCtxtBranch[Namespace,Var,Tag](
//   override val nameSpace : Namespace,
//   val factuals : List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
// ) extends CTreeSection[Either[Tag,Var]]( factuals )
// with AbstractCnxnCtxtBranch[Namespace,Var,Tag]
// with Factual {
//   override def labels : List[CnxnCtxtLabel[Namespace,Var,Tag]] = {
//     factuals
//   }
// }

object CnxnCtxtBranch {
  def unapply[Namespace,Var,Tag](
    cnxnCtxtBranch : CnxnCtxtBranch[Namespace,Var,Tag]
  ) : Option[
	(
	  Namespace,
	  List[CnxnCtxtLabel[Namespace,Var,Tag] with Factual]
	)
      ] = {
    Some( ( cnxnCtxtBranch.nameSpace, cnxnCtxtBranch.factuals
 ) )
  }
}

trait CnxnCtxtInjector[Namespace,Var,Tag] {
  def injectLabel( cLabel : CnxnLabel[Namespace,Tag] )
  : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    cLabel match {
      case cLeaf : CnxnLeaf[Namespace,Tag] =>
	inject( cLeaf )
      case cBranch : CnxnBranch[Namespace,Tag] =>
	inject( cBranch )
      // case cCLeaf : CCnxnLeaf[Namespace,Tag] =>
// 	inject( cCLeaf )
//       case cCBranch : CCnxnBranch[Namespace,Tag] =>
// 	inject( cCBranch )
    }
  }
  def inject( cLabel : CnxnLeaf[Namespace,Tag] )
  : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    new CnxnCtxtLeaf( Left( cLabel.tag ) )
  }
  def inject( cLabel : CnxnBranch[Namespace,Tag] )
  : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
    new CnxnCtxtBranch(
      cLabel.nameSpace,
      cLabel.factuals.map( injectLabel( _ ) )
    )
  }
//   def inject( cCLabel : CCnxnLeaf[Namespace,Tag] )
//   : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
//     CCnxnCtxtLeaf( Left( cCLabel.tag ) )
//   }
//   def inject( cCLabel : CCnxnBranch[Namespace,Tag] )
//   : CnxnCtxtLabel[Namespace,Var,Tag] with Factual = {
//     CCnxnCtxtBranch(
//       cCLabel.nameSpace,
//       cCLabel.factuals.map( injectLabel( _ ) )
//     )
//   }
}

trait Cnxn[Src,+Label,Trgt] {
  def src   : Src
  def label : Label
  def trgt  : Trgt
}
class CCnxn[Src,+Label,Trgt](
  override val src : Src,
  override val label : Label,
  override val trgt : Trgt
) extends Cnxn[Src,Label,Trgt]

object CCnxn {
  def apply[Src,Label,Trgt](
    src : Src, lbl : Label, trgt : Trgt 
  ) : CCnxn[Src,Label,Trgt] = {
    new CCnxn[Src,Label,Trgt]( src, lbl, trgt )
  }
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
  override val labels : List[CnxnLabel[String,String] with Factual]
) extends CnxnBranch[String,String]( nameSpace, labels )

