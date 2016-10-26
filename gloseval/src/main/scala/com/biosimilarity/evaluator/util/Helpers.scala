package com.biosimilarity.evaluator.util

import scala.util.{Failure, Success, Try}

trait Helpers {

  implicit class RichOption[T](o: Option[T]) {
    def toTry: Try[T] = o match {
      case Some(x) => Success(x)
      case None    => Failure(new Exception)
    }
  }

  // format: off
  implicit class RichListTry[A](t: List[Try[A]]) {

    def sequence: Try[List[A]] =
      Try {
        t.map {
          case Success(x)  => x
          case Failure(ex) => throw ex
        }
      }

    def traverse[B](f: A => Try[B]): Try[List[B]] =
      t.map {
        case Success(x)  => f(x)
        case Failure(ex) => Failure(ex)
      }.sequence
  }
  // format: on
}
