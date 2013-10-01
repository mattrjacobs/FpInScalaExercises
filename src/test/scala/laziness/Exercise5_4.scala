package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_4 extends Specification {
  import Stream._

  lazy val grenade: Int = {
    if (1 - 1 == 0) {
      throw new RuntimeException("blow up the stream")
    } else {
      0
    }
  }

  val stream = cons(1, cons(2, cons(3, cons(4, Stream.empty))))
  val explodingStream = cons(1, cons(2, cons(grenade, Stream.empty)))

  def is =
    "Exercise 5-4" ^
      "Stream.forAll" ^
      "true" ! forAllTrue ^
      "false" ! forAllSecondFalse ^
      "exploding doesn't get referenced" ! forAllSecondFalseThirdExplodes ^
      "exploding does get references" ! forAllTrueThenExplodes

  def forAllTrue = stream.forAll(n => n > 0) must beTrue

  def forAllSecondFalse = stream.forAll(n => n != 2) must beFalse

  def forAllSecondFalseThirdExplodes =
    explodingStream.forAll(n => n != 2) must beFalse

  def forAllTrueThenExplodes =
    explodingStream.forAll(n => n > 0) must throwA[RuntimeException]
}
