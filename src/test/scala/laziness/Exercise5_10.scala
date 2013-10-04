package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_10 extends Specification {
  import Stream._

  def is =
    "Exercise 5-9 (Stream.fibs)" ^
      "take some" ^
      "take 0" ! take0 ^
      "take 10" ! take10

  def take0 = Stream.fibs.take(0).toList must_== Nil

  def take10 = Stream.fibs.take(10).toList must_== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
}
