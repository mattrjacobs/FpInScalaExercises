package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_2 extends TestRng {

  def is =
    "Exercise 6-2" ^
      "RNG.double" ^
      "returns double in [0, 1]" ! randomDouble

  def randomDouble = {
    val d = rng.double.run(rng)._1
    println("Random double : " + d)
    doubleOk(d)
  }
}
