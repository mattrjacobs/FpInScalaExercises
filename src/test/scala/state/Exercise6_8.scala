package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_8 extends TestRng {

  def is =
    "Exercise 6-8" ^
      "RNG.positiveLessThan(5)" ^
      "returns an int less than 5" ! lessThan5 ^
      p ^
      "RNG.positiveLessThan(900)" ^
      "returns an int less than 900" ! lessThan900

  def lessThan5 = {
    val (i, r) = rng.positiveLessThan(5)(rng)
    println(" < 5 : " + i)
    i must beLessThan(5)
  }

  def lessThan900 = {
    val (i, r) = rng.positiveLessThan(900)(rng)
    println(" < 900 : " + i)
    i must beLessThan(900)
  }

}
