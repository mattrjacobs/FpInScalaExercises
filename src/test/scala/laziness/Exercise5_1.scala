package com.mattrjacobs.fp

import org.specs2._

class Exercise5_1 extends Specification {
  def is =
    "Exercise 5-1" ^
      "Stream.toList" ^
      "empty Stream" ! emptyStream ^
      "nonEmptyStream" ! nonEmptyStream

  def emptyStream = {
    val stream = Stream.empty
    stream.toList must_== Nil
  }

  def nonEmptyStream = {
    import Stream._
    val stream = cons(1, cons(2, cons(3, Stream.empty)))
    stream.toList must_== List(1, 2, 3)
  }
}
