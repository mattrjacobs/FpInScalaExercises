package com.mattrjacobs.fp

import org.specs2._

class Exercise12 extends Specification {
  def is =
    "Exercise12" ^
      "non-tailrec Fibonacci" ^
      "1" ! nonTailRecFib1 ^
      "5" ! nonTailRecFib5 ^
      "10" ! nonTailRecFib10 ^
      p ^
      "tailrec Fibonacci" ^
      "1" ! tailRecFib1 ^
      "5" ! tailRecFib5 ^
      "10" ! tailRecFib10

  //0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
  def nonTailRecFib1 = Recursive.nonTailRecFib(1) must_== 0
  def nonTailRecFib5 = Recursive.nonTailRecFib(5) must_== 3
  def nonTailRecFib10 = Recursive.nonTailRecFib(10) must_== 34
  def tailRecFib1 = Recursive.tailRecFib(1) must_== 0
  def tailRecFib5 = Recursive.tailRecFib(5) must_== 3
  def tailRecFib10 = Recursive.tailRecFib(10) must_== 34
}
