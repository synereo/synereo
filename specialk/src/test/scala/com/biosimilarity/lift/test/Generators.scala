package com.biosimilarity.lift.test

import org.scalacheck.{Arbitrary, Gen}

object Generators {

  val integersGreaterThanZero              = Gen.choose(1, Int.MaxValue)
  val nonEmptySetOfIntegersGreaterThanZero = Gen.nonEmptyContainerOf[Set, Int](integersGreaterThanZero)
  val nonEmptyString                       = Gen.nonEmptyContainerOf[List, Char](Gen.alphaChar).map(_.mkString)
  val nonEmptyLowercaseString              = Gen.nonEmptyContainerOf[List, Char](Gen.alphaChar).map(_.mkString).map(_.toLowerCase)
  val nonEmptySetOfNonEmptyStrings         = Gen.nonEmptyContainerOf[Set, String](nonEmptyString)
  val nonEmptyArrayOfNonEmptyStrings       = Gen.nonEmptyContainerOf[Array, String](nonEmptyString)
  val nonEmptyByteArray                    = Gen.nonEmptyContainerOf[Array, Byte](Arbitrary.arbitrary[Byte])
  val nonEmptySetOfNonEmptyByteArrays      = Gen.nonEmptyContainerOf[Set, Array[Byte]](nonEmptyByteArray)
  val nonEmptyArrayOfNonEmptyByteArrays    = Gen.nonEmptyContainerOf[Array, Array[Byte]](nonEmptyByteArray)

  val sentenceLike = Gen.nonEmptyContainerOf[List, String](nonEmptyString).map(_.mkString(" "))

  val nonEmptySetOfNonEmptyListsOfNonEmptyStrings =
    Gen.nonEmptyContainerOf[Set, List[String]](Gen.nonEmptyContainerOf[List, String](nonEmptyString))

  val nonEmptySetOfNonEmptyListsOfNonEmptyByteArrays =
    Gen.nonEmptyContainerOf[Set, List[Array[Byte]]](Gen.nonEmptyContainerOf[List, Array[Byte]](nonEmptyByteArray))

  val mapOfSentenceLikeToSentenceLike = Gen.nonEmptyMap[String, String](for {
    x <- sentenceLike
    y <- sentenceLike
  } yield (x, y))
}
