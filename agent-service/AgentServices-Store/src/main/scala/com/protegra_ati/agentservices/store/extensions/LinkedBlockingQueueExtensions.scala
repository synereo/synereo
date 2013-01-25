package com.protegra_ati.agentservices.store.extensions

import actors.threadpool._

/* User: jklassen
*/

object LinkedBlockingQueueExtensions
{
  implicit def linkedBlockingQueueExt[A](source: LinkedBlockingQueue[A]) = new LinkedBlockingQueueExt[A](source)

  class LinkedBlockingQueueExt[A](source: LinkedBlockingQueue[A])
  {

    def containsValue(query: A): Option[A] =
    {
      source.contains(query) match {
        case true => Some(query)
        case _ => None
      }
    }
  }

}