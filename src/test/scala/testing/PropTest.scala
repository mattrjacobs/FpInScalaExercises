package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class PropTest extends Specification {
  val rng = RNG.simple(3456L)

  def is =
    "Gen.forall" ^
      "with constrained succeeding prop" ! constrainedSuccess ^
      "with constrained failing prop" ! constrainedFailure ^
      "with arbitary succeeding prop" ! arbitrarySuccess ^
      "with arbitrary failing prop" ! arbitraryFailure

  def constrainedSuccess = {
    val gen = Gen.choose(4, 5)
    val prop = Prop.forAll(gen) { i =>
      i == 4
    }
    prop.run(10, 1000, rng) mustEqual None
  }

  def constrainedFailure = {
    val gen = Gen.unit(10)
    val prop = Prop.forAll(gen) { i =>
      i < 6
    }
    prop.run(10, 1000, rng) mustEqual Some((10, "10", 0))
  }

  def arbitrarySuccess = {
    val gen = Gen.choose(1, 6).map(x => x * x)
    val prop = Prop.forAll(gen) {
      case 1  => true
      case 4  => true
      case 9  => true
      case 16 => true
      case 25 => true
      case _  => false
    }
    prop.run(10, 1000, rng) mustEqual None
  }

  def arbitraryFailure = {
    val bigNumber = 10000000
    val gen = Gen.choose(1, bigNumber)
    val prop = Prop.forAll(gen) { i =>
      i < (bigNumber * 0.9)
    }
    val result = prop.run(100, 1000, rng) match {
      case Some((_, s, _)) => s.toInt match {
        case n if n > (bigNumber * 0.9) => true
        case _                          => false
      }
      case _ => false
    }
    result must beTrue
  }
}
