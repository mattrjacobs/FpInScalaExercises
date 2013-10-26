package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_16 extends Specification {
  import Stream._

  lazy val three = cons(1, cons(2, cons(3, Stream.empty)))

  def is =
    "Exercise 5-16" ^
      "Stream.scanRight" ^
      "Stream(1,2,3).scanRight(0)(_ + _) = List(6,5,3,0)" ! scanExample

  def scanExample =
    three.scanRight(0)(_ + _).toList must_== List(6, 5, 3, 0)
}
