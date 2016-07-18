package com.biosimilarity.lift.test

import java.util.UUID

import com.biosimilarity.lift.lib._
import com.biosimilarity.lift.model.store._

/**
  * lifted from CnxnQuery.scala
  */
object CnxnQueryUtilities {

  object CnxnCtxtLabelMatcher
      extends CnxnUnificationTermQuery[String, String, String]
      with CnxnConversions[String, String, String]
      with UUIDOps
      with Serializable

  object LabelStream extends Serializable {

    def uuid(): UUID = UUID.randomUUID()

    def uuidLike(): String = uuid().toString.replace("-", "")

    def tStream[T](seed: T)(fresh: T => T): Stream[T] = {
      lazy val loopStrm: Stream[T] = List(seed).toStream append (loopStrm map fresh)
      loopStrm
    }

    def strings(seed: String)(implicit useUUID: Boolean = false): Stream[String] = {
      def freshString(s: String): String = useUUID match {
        case true => s + uuidLike()
        case false =>
          val a = s.split(seed)
          seed + (a(1).toInt + 1)
      }
      val suffix = useUUID match {
        case true  => uuidLike()
        case false => "1"
      }
      tStream[String](seed + suffix)(freshString)
    }

    type CCL = CnxnCtxtLabel[String, String, String]

    type CCLwF = CCL with Factual

    def atoms(seed: String): Stream[CCLwF] = {
      def leaf: CCLwF =
        new CnxnCtxtLeaf[String, String, String](Left[String, String](uuidLike() + seed))
      def freshTree(tree: CCLwF): CCLwF = leaf
      tStream[CCLwF](leaf)(freshTree)
    }

    def identifiers(seed: String): Stream[CCLwF] = {
      def leaf: CCLwF =
        new CnxnCtxtLeaf[String, String, String](Right[String, String]("X" + uuidLike() + seed))
      def freshTree(tree: CCLwF): CCLwF = leaf
      tStream[CCLwF](leaf)(freshTree)
    }

    def facts(seed: String): Stream[CCLwF] = {
      def leaf: CCLwF = new CnxnCtxtLeaf[String, String, String](Left[String, String](uuidLike() + seed))
      def freshTree(tree: CCLwF): CCLwF =
        new CnxnCtxtBranch[String, String, String](
          "t" + uuidLike(),
          List(new CnxnCtxtBranch[String, String, String]("tLeft" + uuidLike(), List[CCLwF](tree, leaf)),
               new CnxnCtxtBranch[String, String, String]("tRight" + uuidLike(), List[CCLwF](leaf, tree))))
      tStream[CCLwF](leaf)(freshTree)
    }

    def queries(): Stream[CCLwF] = {
      def leaf: CCLwF =
        new CnxnCtxtLeaf[String, String, String](Right[String, String]("X" + uuidLike()))
      def freshTree(tree: CCLwF): CCLwF =
        new CnxnCtxtBranch[String, String, String](
          "t" + uuidLike(),
          List(new CnxnCtxtBranch[String, String, String]("tLeft" + uuidLike(), List[CCLwF](tree, leaf)),
               new CnxnCtxtBranch[String, String, String]("tRight" + uuidLike(), List[CCLwF](leaf, tree))))
      tStream[CCLwF](leaf)(freshTree)
    }
  }
}
