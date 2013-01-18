package com.protegra_ati.agentservices.store.extensions

/* User: jklassen
*/

object OptionExtensions {
  implicit def optionExt[A](source: Option[A]) = new OptionExt(source)

  class OptionExt[A](source: Option[A]) {

    def value = {
      source.getOrElse("")
    }
  }

}