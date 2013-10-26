package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_15 extends Specification {
  import Stream._

  lazy val zero = Stream.empty[Int]
  lazy val one = cons(1, Stream.empty)
  lazy val three = cons(1, cons(2, cons(3, Stream.empty)))

  def is =
    "Exercise 5-15" ^
      "Stream.tails" ^
      "empty.tails" ! emptyTails ^
      "one.tails" ! oneTails ^
      "three.tails" ! threeTails

  def emptyTails =
    zero.tails.map(_.toList).toList must_== List(Nil)

  def oneTails =
    one.tails.map(_.toList).toList must_== List(List(1), Nil)

  def threeTails =
    three.tails.map(_.toList).toList must_== List(List(1, 2, 3), List(2, 3), List(3), Nil)
}
