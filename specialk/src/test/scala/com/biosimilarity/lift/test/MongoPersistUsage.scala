package com.biosimilarity.lift.test

import com.biosimilarity.lift.model.store.CnxnCtxtLabel
import com.biosimilarity.lift.model.store.mongo.BaseMongoPersist

/**
  * lifted from MongoPersist.scala
  */
class MongoPersistUsage {

  object DataSet {

    val numberStr      = """number( 7329 )"""
    val streetStr      = """street( "39th Ave" )"""
    val cityStr        = """city( "Seattle" )"""
    val stateStr       = """state( "WA" )"""
    val countryStr     = """country( "US")"""
    val addressStr     = s"address( $numberStr , $streetStr , $cityStr , $stateStr , $countryStr )"
    val profileStr     = s"profile( $addressStr )"
    val userProfileStr = s"user( $profileStr )"
    val dataBlob       = s"""|object Pi {
                             |  class PiIterator extends Iterable[BigInt] {
                             |    var r: BigInt = 0
                             |    var q, t, k: BigInt = 1
                             |    var n, l: BigInt = 3
                             |    var nr, nn: BigInt = 0
                             |
                             |    def iterator: Iterator[BigInt] = new Iterator[BigInt] {
                             |      def hasNext = true
                             |      def next(): BigInt = {
                             |        while((4*q+r-t) >= (n*t)) {
                             |          nr = (2*q+r)*l
                             |          nn = (q*(7*k)+2+(r*l))/(t*l)
                             |          q = q * k
                             |          t = t * l
                             |          l = l + 2
                             |          k = k + 1
                             |          n  = nn
                             |          r  = nr
                             |        }
                             |        val ret = n
                             |        nr = 10*(r-n*t)
                             |        n  = ((10*(3*q+r))/t)-(10*n)
                             |        q  = q * 10
                             |        r  = nr
                             |        ret
                             |      }
                             |  }
                             |}""".stripMargin
  }

  object SimpleMongoPersist extends BaseMongoPersist[String, String, String] {
    import DataSet._
    val userProfileCCL: CnxnCtxtLabel[String, String, String] = userProfileStr
  }
}
