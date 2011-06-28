// -*- mode: Scala;-*- 
// Filename:    AttrTreeComonad.scala 
// Authors:     lgm                                                    
// Creation:    Tue Jun 28 06:11:47 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.lib.comonad

import com.biosimilarity.lift.lib.zipper._

trait AttributedTreeComonadScope[Item] {
  type ATree[Attr] = AttributedTree[Attr,Item]
  class AttrTreeCM[A]( )
  extends BComonad[ATree] {
    override def counit [S] ( tS : ATree[S] ) : S = 
      tS.attribute
    override def cobind [S,T] (
      ctxt : ATree[S] => T, 
      tS : ATree[S] 
    ) : ATree[T] = {
      val t : T = ctxt( tS )
      tS match {
	case tI@AttrTreeItem( _, attrItem ) => {
	  new AttrTreeItem( 
	    t,
	    attrItem
	  )
	}
	case tI@AttrTreeSection( _, attrSection ) => {
	  new AttrTreeSection( 
	    t,
	    attrSection.map(
	      ( atree : ATree[S] ) => {
		cobind( ctxt, atree )
	      }
	    )
	  )
	}
      }
    }
  }
}
