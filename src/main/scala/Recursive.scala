package com.mattrjacobs.fp

object Recursive {
  def nonTailRecFib(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case n => nonTailRecFib(n - 1) + nonTailRecFib(n - 2)
  }

  def tailRecFib(n: Int): Int = {
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

  //foo(0, 1, 8) ->
  //foo(1, 1, 7) ->
  //foo(1, 2, 6) ->
  //foo(2, 3, 5) ->
  //foo(3, 5, 4) ->
  //foo(5, 8, 3) ->
  //foo(8, 13, 2) ->
  //foo(13, 21, 1) ->
  //foo(21, 34, 0)

}
