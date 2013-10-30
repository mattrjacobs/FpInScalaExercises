package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_3 extends TestRng {

  def is =
    "Exercise 6-3" ^
      "RNG.intDouble" ^
      "returns valid int and double" ! randomIntDouble ^
      p ^
      "RNG.doubleInt" ^
      "returns valid double and int" ! randomDoubleInt ^
      p ^
      "RNG.double3" ^
      "returns 3 valid doubles" ! randomDouble3

  def randomIntDouble = {
    val ((i, d), r) = rng.intDouble(rng)
    println("Random int : " + i)
    println("Random double : " + d)
    doubleOk(d)
  }

  def randomDoubleInt = {
    val ((d, i), r) = rng.doubleInt(rng)
    println("Random double : " + d)
    println("Random int : " + i)
    doubleOk(d)
  }

  def randomDouble3 = {
    val ((d1, d2, d3), r) = rng.double3(rng)
    println("Random double 1 : " + d1)
    println("Random double 2 : " + d2)
    println("Random double 3 : " + d3)
    doubleOk(d1)
    doubleOk(d2)
    doubleOk(d3)
  }
}
