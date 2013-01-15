package com.mattrjacobs.fp

object Recursive {
  def nonTailRecFib(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case n => nonTailRecFib(n - 1) + nonTailRecFib(n - 2)
  }

  def tailRecFib(n: Int): Int = {
    //foo(0, 1, 8) ->
    //foo(1, 1, 7) ->
    //foo(1, 2, 6) ->
    //foo(2, 3, 5) ->
    //foo(3, 5, 4) ->
    //foo(5, 8, 3) ->
    //foo(8, 13, 2) ->
    //foo(13, 21, 1) ->
    //foo(21, 34, 0) -> 34
    @scala.annotation.tailrec
    def foo(first: Int, second: Int, num: Int): Int = num match {
      case 0 => second
      case _ => foo(second, first + second, num - 1)
    }

    n match {
      case negative if negative < 0 => 0
      case 0                        => 0
      case 1                        => 0
      case 2                        => 1
      case _                        => foo(0, 1, n - 2)
    }
  }

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n
    val done = iterateWhile(2.0)(x => x - f(x) / (2 * x),
      x => f(x).abs > 1e-14)
    println("SQRT(" + n + ") = " + done)
    done
  }

  @scala.annotation.tailrec
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = p(a) match {
    case true  => iterateWhile(f(a))(f, p)
    case false => a
  }
}
