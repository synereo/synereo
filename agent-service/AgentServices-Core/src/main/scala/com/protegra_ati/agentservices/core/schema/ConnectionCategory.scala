/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.schema

//string list for Enum constructor is required for serialization
//val must be kept in same order as enum constructor
//lower case necessary for prolog label eval (camelcase)
object ConnectionCategory extends Enumeration()
{
  type ConnectionCategory = Value
  val Self, App, Business, Person, Group, Affiliate, None = Value
}
