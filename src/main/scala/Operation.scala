package com.mattrjacobs.fp

object Operation {
  type Pred[A] = A => Boolean

  def evenDirect(n: Int): Boolean = n % 2 == 0
  def even(n: Int): Boolean = divisibleBy(n)(2)
  def negative(n: Int): Boolean = n < 0
  def odd_1(n: Int): Boolean = !(even(n))
  def positive_1(n: Int): Boolean = !(negative(n))
  def notMonomorphic(p: Pred[Int]): Pred[Int] =
    n => !(p(n))
  def not[A](p: Pred[A]): Pred[A] = n => !(p(n))
  val odd = not[Int](even)
  val positive = not[Int](negative)
  def abs(n: Int) = negative(n) match {
    case true  => -n
    case false => n
  }
  def absoluteMonomorphic(p: Int => Int): Int => Int =
    n => abs(p(n))
  def absolute[A](p: A => Int): A => Int =
    n => abs(p(n))
  def divisibleBy(k: Int): Pred[Int] =
    n => k % n == 0
}
