package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_2 extends Specification {

  val rng = RNG.simple(12345L)

  def is =
    "Exercise 6-2" ^
      "RNG.double" ^
      "returns double in [0, 1]" ! randomDouble

  def randomDouble = {
    val d = rng.double(rng)._1
    println("Random double : " + d)
    d must beLessThanOrEqualTo(1.0)
    d must beGreaterThanOrEqualTo(0.0)
  }
}
