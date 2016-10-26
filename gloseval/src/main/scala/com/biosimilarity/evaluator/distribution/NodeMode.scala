package com.biosimilarity.evaluator.distribution

sealed trait NodeMode
case object Headed extends NodeMode {
  override def toString: String = "headed"
}
case object Headless extends NodeMode {
  override def toString: String = "headless"
}
