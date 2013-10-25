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
      "three.zip(three) => three" ! threeZipThree ^
      p ^
      "Stream.zipAll" ^
      "empty.zipAll(empty)" ! emptyZipAllEmpty ^
      "one.zipAll(empty)" ! oneZipAllEmpty ^
      "empty.zipAll(one)" ! emptyZipAllOne ^
      "one.zipAll(one)" ! oneZipAllOne ^
      "one.zipAll(three)" ! oneZipAllThree ^
      "three.zipAll(one)" ! threeZipAllOne ^
      "three.zipAll(three)" ! threeZipAllThree

  def emptyZipEmpty =
    zero.zip(zero).toList must_== Nil

  def emptyZipAllEmpty =
    zero.zipAll(zero).toList must_== Nil

  def oneZipEmpty =
    one.zip(zero).toList must_== Nil

  def oneZipAllEmpty =
    one.zipAll(zero).toList must_== List((Some(1), None))

  def emptyZipOne =
    zero.zip(one).toList must_== Nil

  def emptyZipAllOne =
    zero.zipAll(one).toList must_== List((None, Some(1)))

  def oneZipOne =
    one.zip(one).toList must_== List((1, 1))

  def oneZipAllOne =
    one.zipAll(one).toList must_== List((Some(1), Some(1)))

  def oneZipThree =
    one.zip(three).toList must_== List((1, 1))

  def oneZipAllThree =
    one.zipAll(three).toList must_== List((Some(1), Some(1)), (None, Some(2)), (None, Some(3)))

  def threeZipOne =
    three.zip(one).toList must_== List((1, 1))

  def threeZipAllOne =
    three.zipAll(one).toList must_== List((Some(1), Some(1)), (Some(2), None), (Some(3), None))

  def threeZipThree =
    three.zip(three).toList must_== List((1, 1), (2, 2), (3, 3))

  def threeZipAllThree =
    three.zipAll(three).toList must_== List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)))
}
