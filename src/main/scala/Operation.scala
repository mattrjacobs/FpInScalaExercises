package com.mattrjacobs.fp

object Operation {
  type Pred[A] = A => Boolean

  def evenDirect(n: Int): Boolean = n % 2 == 0
  def even(n: Int): Boolean = divisibleBy(2)(n)
  def negative(n: Int): Boolean = n < 0
  def odd_1(n: Int): Boolean = !(even(n))
  def positive_1(n: Int): Boolean = !(negative(n))
  def notMonomorphic(p: Pred[Int]): Pred[Int] =
    n => !(p(n))
  def not[A](p: Pred[A]): Pred[A] = n => !(p(n))
  val odd = not[Int](even)
  val positive = not[Int](negative)

  def absoluteMonomorphic(p: Int => Int): Int => Int = p(_).abs
  def absolute[A](p: A => Int): A => Int = p(_).abs
  def divisibleBy(k: Int): Pred[Int] = _ % k == 0
  val divisibleBy3And5Simple: Pred[Int] =
    n => divisibleBy(3)(n) && divisibleBy(5)(n)
  val divisibleBy3Or5Simple: Pred[Int] =
    n => divisibleBy(3)(n) || divisibleBy(5)(n)
  val divisibleBy3And5: Pred[Int] =
    divisibleWith3And5(_ && _)
  val divisibleBy3Or5: Pred[Int] =
    divisibleWith3And5(_ || _)
  def divisibleWith3And5(f: (Boolean, Boolean) => Boolean) =
    liftBool[Int](f, divisibleBy(3), divisibleBy(5))

  def liftBool[A](f: (Boolean, Boolean) => Boolean, g: Pred[A], h: Pred[A]): Pred[A] =
    n => f(g(n), h(n))

  def lift[A, B, C, D](f: (B, C) => D)(g: A => B, h: A => C): A => D =
    n => f(g(n), h(n))

  def lift3[A, B, C, D, E](f: (B, C, D) => E)(g: A => B, h: A => C, i: A => D): A => E =
    n => f(g(n), h(n), i(n))

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    case (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    n => f(g(n))
}
