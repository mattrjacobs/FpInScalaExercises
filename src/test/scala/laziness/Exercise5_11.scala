package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_11 extends Specification {
  import Stream._

  def f(s: Stream[Int]) = s.uncons

  def g(s: Stream[Int]) = s.uncons match {
    case Some((h, t)) if h <= 5 => Some((h, t))
    case _                      => None
  }

  def is =
    "Exercise 5-11" ^
      "Stream.unfold" ^
      "unfold(ints)(_ => None)" ! unfoldNone ^
      "unfold(ints)(_ => Some).take0" ! unfoldSomeTake0 ^
      "unfold(ints)(_ => Some).take10" ! unfoldSomeTake10 ^
      "unfold(ints)(_ => Some to 5).take0" ! unfoldSomeTo5Take0 ^
      "unfold(ints)(_ => Some to 5).take10" ! unfoldSomeTo5Take10

  def unfoldNone = Stream.unfold(from(1))(_ => None).toList must_== Nil

  def unfoldSomeTake0 = Stream.unfold(from(1))(f).take(0).toList must_== Nil

  def unfoldSomeTake10 = Stream.unfold(from(1))(f).take(10).toList must_== (1 to 10)

  def unfoldSomeTo5Take0 = Stream.unfold(from(1))(g).take(0).toList must_== Nil

  def unfoldSomeTo5Take10 = Stream.unfold(from(1))(g).take(10).toList must_== (1 to 5)
}
