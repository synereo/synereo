// -*- mode: Scala;-*- 
// Filename:    View.scala 
// Authors:     lgm                                                    
// Creation:    Wed Jul  6 16:42:16 2011 
// Copyright:   Not supplied 
// Description: 
// ------------------------------------------------------------------------

package com.biosimilarity.lift.model.store

trait View {
  def toKey [Namespace,Var,Tag,Value] (
    v : Value
  ) : CnxnCtxtLabel[Namespace,Var,Tag]
}

trait ViewXForm {
  def xform ( v : View ) : View
}
