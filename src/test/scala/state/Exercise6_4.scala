package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_4 extends TestRng {

  def is =
    "Exercise 6-4" ^
      "RNG.ints(0)" ^
      "returns Nil and the same rng" ! ints0 ^
      p ^
      "RNG.ints(4)" ^
      "returns 4 random ints and a new rng" ! ints4

  def ints0 = {
    val (l, r) = RNG.ints(0).run(rng)
    l must beEmpty
    r must beEqualTo(rng)
  }

  def ints4 = {
    val (l, r) = RNG.ints(4).run(rng)
    l.foreach(i => println("Random int from list : " + i))
    l must haveSize(4)
  }
}
