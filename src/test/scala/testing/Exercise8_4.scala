package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class Exercise8_4 extends Specification {

  val rng = RNG.simple(1234L)

  def is =
    "Exercise 8-4" ^
      "Gen.choose" ^
      "returns all values in the specified range" ! choose

  def choose = {
    val chooser = Gen.choose(3, 19)

    val (_, overallBool) = (1 to 100).foldLeft((rng, true)) { (tuple, _) =>
      val (r, boolSoFar) = tuple
      val (i, newRng) = chooser.sample.run(r)
      val newBoolSoFar = boolSoFar && (i >= 3) && (i < 19)
      (newRng, newBoolSoFar)
    }
    overallBool must beTrue
  }
}
