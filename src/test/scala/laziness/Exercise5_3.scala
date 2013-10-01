package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_3 extends Specification {
  import Stream._

  val stream = cons(1, cons(2, cons(3, cons(4, Stream.empty))))

  def is =
    "Exercise 5-3" ^
      "Stream.takeWhile" ^
      "true" ! takeAll ^
      "even" ! takeEven ^
      "odd" ! takeOdd

  def takeAll = stream.takeWhile(_ => true).take(4).toList must_== List(1, 2, 3, 4)

  def takeEven = stream.takeWhile(n => n % 2 == 0).take(2).toList must_== List(2, 4)

  def takeOdd = stream.takeWhile(n => n % 2 == 1).take(2).toList must_== List(1, 3)
}
