package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_13 extends Specification {
  import Stream._

  lazy val zero = Stream.empty[Int]
  lazy val one = cons(1, Stream.empty)
  lazy val three = cons(1, cons(2, cons(3, Stream.empty)))

  def is =
    "Exercise 5-13" ^
      "Stream.zip" ^
      "empty.zip(empty) => empty" ! emptyZipEmpty ^
      "one.zip(empty) => empty" ! oneZipEmpty ^
      "empty.zip(one) => empty" ! emptyZipOne ^
      "one.zip(one) => one" ! oneZipOne ^
      "one.zip(three) => one" ! oneZipThree ^
      "three.zip(one) => one" ! threeZipOne ^
      "three.zip(three) => three" ! threeZipThree

  def emptyZipEmpty =
    zero.zip(zero).toList must_== Nil

  def oneZipEmpty =
    one.zip(zero).toList must_== Nil

  def emptyZipOne =
    zero.zip(one).toList must_== Nil

  def oneZipOne =
    one.zip(one).toList must_== List((1, 1))

  def oneZipThree =
    one.zip(three).toList must_== List((1, 1))

  def threeZipOne =
    three.zip(one).toList must_== List((1, 1))

  def threeZipThree =
    three.zip(three).toList must_== List((1, 1), (2, 2), (3, 3))
}
