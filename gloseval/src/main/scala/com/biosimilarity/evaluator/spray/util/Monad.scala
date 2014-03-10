package com.biosimilarity.evaluator.spray.util

trait MonadT[M[_], A] {
  // fundamental
  def map[B](f: A => B): M[B]
  def unit[B](b: B): M[B]
  def flatten[B](mmb: M[M[B]]): M[B]

  // derived
  def flatMap[B](f: A => M[B]): M[B] = flatten(map[M[B]](f))
  def foreach[B](f: A => B): Unit = map((a: A) => { f(a); () })
}