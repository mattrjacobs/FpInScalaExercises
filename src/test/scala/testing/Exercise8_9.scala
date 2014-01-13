package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._
import org.specs2.matcher._

class Exercise8_9 extends Specification with ThrownExpectations {

  val rng = RNG.simple(1234L)

  val even = Gen.choose(0, 51).map(_ * 2)
  val odd = Gen.choose(0, 50).map(x => (x * 2) + 1)

  val trueProp1 = Gen.forAll(even) {
    i => i % 2 == 0
  }

  val trueProp2 = Gen.forAll(even) {
    i => i < 1000
  }

  val falseProp1 = Gen.forAll(even) {
    i => i % 2 == 1
  }

  val falseProp2 = Gen.forAll(even) {
    i => i > 12121
  }

  def is =
    "Exercise 8-9" ^
      "Prop.and" ^
      "both true" ! bothTrueAnd ^
      "one true" ! oneTrueAnd ^
      "both false" ! bothFalseAnd ^
      p ^
      "Prop.or" ^
      "both true" ! bothTrueOr ^
      "one true" ! oneTrueOr ^
      "both false" ! bothFalseOr

  def bothTrueAnd = {
    trueProp1.run(100, rng) mustEqual None
    trueProp2.run(100, rng) mustEqual None

    (trueProp1 && trueProp2).run(100, rng) mustEqual None
  }

  def oneTrueAnd = {
    trueProp1.run(100, rng) mustEqual None
    falseProp1.run(100, rng).isDefined mustEqual true

    (trueProp1 && falseProp1).run(100, rng).isDefined mustEqual true
  }

  def bothFalseAnd = {
    falseProp1.run(100, rng).isDefined mustEqual true
    falseProp2.run(100, rng).isDefined mustEqual true

    val result = (falseProp1 && falseProp2).run(100, rng)
    result.isDefined mustEqual true
  }

  def bothTrueOr = {
    trueProp1.run(100, rng) mustEqual None
    trueProp2.run(100, rng) mustEqual None

    (trueProp1 || trueProp2).run(100, rng) mustEqual None
  }

  def oneTrueOr = {
    trueProp1.run(100, rng) mustEqual None
    falseProp1.run(100, rng).isDefined mustEqual true

    (trueProp1 || falseProp1).run(100, rng) mustEqual None
  }

  def bothFalseOr = {
    falseProp1.run(100, rng).isDefined mustEqual true
    falseProp2.run(100, rng).isDefined mustEqual true

    val result = (falseProp1 || falseProp2).run(100, rng)
    result.isDefined mustEqual true
  }
}
