package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_14 extends Specification {
  import Stream._

  lazy val zero = Stream.empty[Int]
  lazy val one = cons(1, zero)
  lazy val two = cons(1, cons(2, zero))
  lazy val three = cons(1, cons(2, cons(3, zero)))

  def is =
    "Exercise 5-14" ^
      "Stream.startsWith" ^
      "empty.startsWith(empty) isTrue" ! emptyStartsWithEmpty ^
      "empty.startsWith(1) isFalse" ! emptyStartsWithOne ^
      "Stream(1).startsWith(empty) isTrue" ! oneStartsWithEmpty ^
      "Stream(1).startsWith(1) isTrue" ! oneStartsWithOne ^
      "Stream(1).startsWith(3) isFalse" ! oneStartsWithThree ^
      "Stream(1, 2).startsWith(empty) isTrue" ! oneTwoStartsWithEmpty ^
      "Stream(1, 2).startsWith(1) isTrue" ! oneTwoStartsWithOne ^
      "Stream(1, 2).startsWith(1, 2) isTrue" ! oneTwoStartsWithOneTwo ^
      "Stream(1, 2).startsWith(1, 3) isFalse" ! oneTwoStartsWithOneThree ^
      "Stream(1, 2).startsWith(1, 2, 3) isFalse" ! oneTwoStartsWithOneTwoThree

  def emptyStartsWithEmpty =
    Stream.startsWith(zero, zero) must beTrue

  def emptyStartsWithOne =
    Stream.startsWith(zero, one) must beFalse

  def oneStartsWithEmpty =
    Stream.startsWith(one, zero) must beTrue

  def oneStartsWithOne =
    Stream.startsWith(one, one) must beTrue

  def oneStartsWithThree =
    Stream.startsWith(one, cons(3, zero)) must beFalse

  def oneTwoStartsWithEmpty =
    Stream.startsWith(two, zero) must beTrue

  def oneTwoStartsWithOne =
    Stream.startsWith(two, one) must beTrue

  def oneTwoStartsWithOneTwo =
    Stream.startsWith(two, two) must beTrue

  def oneTwoStartsWithOneThree =
    Stream.startsWith(two, cons(1, cons(3, zero))) must beFalse

  def oneTwoStartsWithOneTwoThree =
    Stream.startsWith(two, three) must beFalse
}
