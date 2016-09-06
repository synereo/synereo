package com.biosimilarity.lift

package object test {

  def genRandQueueName(): String = s"test_${java.util.UUID.randomUUID()}"
}
