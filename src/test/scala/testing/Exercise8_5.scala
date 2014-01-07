package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class Exercise8_5 extends Specification {

  val rng = RNG.simple(1234L)
  val num = 100

  def is =
    "Exercise 8-5" ^
      "Gen.unit" ^
      "returns a constant value" ! unit ^
      p ^
      "Gen.boolean" ^
      "return a random boolean" ! bool ^
      p ^
      "Gen.listOfN" ^
      "returns a list of proper size and elements in proper range" ! list

  def unit = {
    val u = Gen.unit(47)

    val (_, overallBool, finalList) = (1 to num).foldLeft((rng, true, List.empty[Int])) { (tuple, _) =>
      val (r, boolSoFar, l) = tuple
      val (i, newRng) = u.sample.run(r)
      val newBoolSoFar = boolSoFar && (i == 47)
      (newRng, newBoolSoFar, i +: l)
    }
    (overallBool must beTrue) and (finalList.size mustEqual num)
  }

  def bool = {
    val b = Gen.boolean

    val (_, overallBool, finalSet) = (1 to num).foldLeft((rng, true, Set.empty[Boolean])) { (tuple, _) =>
      val (r, boolSoFar, s) = tuple
      val (i, newRng) = b.sample.run(r)
      val newBoolSoFar = boolSoFar && (i || !i)
      (newRng, newBoolSoFar, s + i)
    }
    (overallBool must beTrue) and (finalSet mustEqual Set(true, false))
  }

  def list = {
    val genList = Gen.listOfN(num, Gen.choose(56, 176))

    val (_, overallBool, finalLol) = (1 to num).foldLeft((rng, true, List.empty[List[Int]])) { (tuple, _) =>
      val (r, boolSoFar, lol) = tuple
      val (l, newRng) = genList.sample.run(r)
      val newBoolSoFar = boolSoFar && l.size == num && l.forall(i =>
        (i >= 56) && (i < 176))
      (newRng, newBoolSoFar, l +: lol)
    }
    (overallBool must beTrue) and (finalLol.size mustEqual num)
  }
}
