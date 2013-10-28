package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_1 extends Specification {

  val rng = RNG.simple(12345L)

  def is =
    "Exercise 6-1" ^
      "RNG.positiveInt" ^
      "returns positive int" ! positiveInt

  def positiveInt = {
    val i = rng.positiveInt(rng)._1
    i must beGreaterThan(0)
  }
}
