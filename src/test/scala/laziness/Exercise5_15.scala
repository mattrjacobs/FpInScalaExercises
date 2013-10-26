package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_15 extends Specification {
  import Stream._

  lazy val zero = Stream.empty[Int]
  lazy val one = cons(1, Stream.empty)
  lazy val two = cons(1, cons(2, Stream.empty))
  lazy val three = cons(1, cons(2, cons(3, Stream.empty)))

  def is =
    "Exercise 5-15" ^
      "Stream.tails" ^
      "empty.tails" ! emptyTails ^
      "one.tails" ! oneTails ^
      "three.tails" ! threeTails ^
      p ^
      "Stream.hasSubsequence" ^
      "empty.hasSubsequence(empty) isTrue" ! emptyHasSubsequenceEmpty ^
      "empty.hasSubsequence(1) isFalse" ! emptyHasSubsequenceOne ^
      "Stream(1).hasSubsequence(empty) isTrue" ! oneHasSubsequenceEmpty ^
      "Stream(1).hasSubsequence(1) isTrue" ! oneHasSubsequenceOne ^
      "Stream(1).hasSubsequence(3) isFalse" ! oneHasSubsequenceThree ^
      "Stream(1, 2).hasSubsequence(empty) isTrue" ! oneTwoHasSubsequenceEmpty ^
      "Stream(1, 2).hasSubsequence(1) isTrue" ! oneTwoHasSubsequenceOne ^
      "Stream(1, 2).hasSubsequence(1, 2) isTrue" ! oneTwoHasSubsequenceOneTwo ^
      "Stream(1, 2).hasSubsequence(1, 3) isFalse" ! oneTwoHasSubsequenceOneThree ^
      "Stream(1, 2).hasSubsequence(1, 2, 3) isFalse" ! oneTwoHasSubsequenceOneTwoThree ^
      "Stream(1, 2).hasSubsequence(2) isTrue" ! oneTwoHasSubsequenceTwo ^
      "Stream(1, 2, 3).hasSubsequence(2, 3) isTrue" ! oneTwoThreeHasSubsequenceTwoThree ^
      "Stream(1, 2, 3).hasSubsequence(3) isTrue" ! oneTwoThreeHasSubsequenceThree ^
      "Stream(1, 2, 3).hasSubsequence(3, 2) isFalse" ! oneTwoThreeHasSubsequenceThreeTwo

  def emptyTails =
    zero.tails.map(_.toList).toList must_== List(Nil)

  def oneTails =
    one.tails.map(_.toList).toList must_== List(List(1), Nil)

  def threeTails =
    three.tails.map(_.toList).toList must_== List(List(1, 2, 3), List(2, 3), List(3), Nil)

  def emptyHasSubsequenceEmpty =
    Stream.hasSubsequence(zero, zero) must beTrue

  def emptyHasSubsequenceOne =
    Stream.hasSubsequence(zero, one) must beFalse

  def oneHasSubsequenceEmpty =
    Stream.hasSubsequence(one, zero) must beTrue

  def oneHasSubsequenceOne =
    Stream.hasSubsequence(one, one) must beTrue

  def oneHasSubsequenceThree =
    Stream.hasSubsequence(one, cons(3, zero)) must beFalse

  def oneTwoHasSubsequenceEmpty =
    Stream.hasSubsequence(two, zero) must beTrue

  def oneTwoHasSubsequenceOne =
    Stream.hasSubsequence(two, one) must beTrue

  def oneTwoHasSubsequenceOneTwo =
    Stream.hasSubsequence(two, two) must beTrue

  def oneTwoHasSubsequenceOneThree =
    Stream.hasSubsequence(two, cons(1, cons(3, zero))) must beFalse

  def oneTwoHasSubsequenceOneTwoThree =
    Stream.hasSubsequence(two, three) must beFalse

  def oneTwoHasSubsequenceTwo =
    Stream.hasSubsequence(two, cons(2, Stream.empty)) must beTrue

  def oneTwoThreeHasSubsequenceTwoThree =
    Stream.hasSubsequence(three, cons(2, cons(3, Stream.empty))) must beTrue

  def oneTwoThreeHasSubsequenceThree =
    Stream.hasSubsequence(three, cons(3, Stream.empty)) must beTrue

  def oneTwoThreeHasSubsequenceThreeTwo =
    Stream.hasSubsequence(three, cons(3, cons(2, Stream.empty))) must beFalse
}

