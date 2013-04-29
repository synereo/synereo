package com.protegra_ati.agentservices.core.schema

object ConnectionCategory extends Enumeration {
  type ConnectionCategory = Value
  val Self, App, Business, Person, Group, Affiliate, None = Value
}
