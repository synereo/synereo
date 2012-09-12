/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.protegra_ati.agentservices.core.messages

//string list for Enum constructor is required for serialization
//val must be kept in same order as enum constructor
object ChannelType extends Enumeration("Request", "Response", "Notification")
{
  type ChannelType = Value
  val Request, Response, Notification = Value
}