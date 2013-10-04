package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_8 extends Specification {
  import Stream._

  def is =
    "Exercise 5-8 (Stream.constants)" ^
      "take some" ^
      "take 0" ! take0 ^
      "take 10" ! take10

  def take0 = Stream.constant("z").take(0).toList must_== Nil

  def take10 = Stream.constant(4).take(10).toList must_== (1 to 10).map(_ => 4)
}
