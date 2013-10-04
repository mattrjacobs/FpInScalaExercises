package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_9 extends Specification {
  import Stream._

  def is =
    "Exercise 5-9 (Stream.from)" ^
      "take some" ^
      "take 0" ! take0 ^
      "take 10" ! take10

  def take0 = Stream.from(4).take(0).toList must_== Nil

  def take10 = Stream.from(4).take(10).toList must_== (4 to 13)
}
