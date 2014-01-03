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

    (1 to 100).foldLeft(rng) { (r, _) =>
      val (i, newRng) = chooser.sample.run(r)
      (i >= 3) mustEqual true
      (i < 19) mustEqual true
      newRng
    }
    0 mustEqual 0
  }
}
