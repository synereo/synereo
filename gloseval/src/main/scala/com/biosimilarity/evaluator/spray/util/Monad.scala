package com.biosimilarity.evaluator.spray.util

trait MonadT[M[_], A] {
  def flatMap[B](f: A => M[B]): M[B]
  def map[B](f: A => B): M[B]
  def foreach[B](f: A => B): Unit
  // def withFilter(f: A => Boolean): MonadT[M, A]
}
