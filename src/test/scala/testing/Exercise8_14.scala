package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class Exercise8_14 extends Specification {

  val rng = RNG.simple(11234L)

  def is =
    "Exercise 8-14" ^
      "List.max" ^
      "using listOf fails" ! maxListOf ^
      "using listOf1 works" ! maxListOf1

  def maxListOf = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf(smallInt)) { l =>
      val max = l.max
      !l.exists(_ > max)
    }
    (Prop.run(maxProp) match {
      case Some((_, actual, _)) if actual contains "List()" => true
      case _ => false
    }) mustEqual true
  }

  def maxListOf1 = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { l =>
      val max = l.max
      !l.exists(_ > max)
    }
    Prop.run(maxProp) mustEqual None
  }
}
