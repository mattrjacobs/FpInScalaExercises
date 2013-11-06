package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_1 extends TestRng {
  def is =
    "Exercise 6-1" ^
      "RNG.positiveInt" ^
      "returns positive int" ! positiveInt

  def positiveInt = {
    val i = rng.positiveInt.run(rng)._1
    println("Random int : " + i)
    i must beGreaterThan(0)
  }
}
