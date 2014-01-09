package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class Exercise8_7 extends Specification {

  val rng = RNG.simple(1234L)

  def is =
    "Exercise 8-7" ^
      "Gen.union" ^
      "one possibility for each side" ! constrainedUnion

  def constrainedUnion = {
    val gen1 = Gen.unit(1)
    val gen19 = Gen.unit(19)
    val gen: Gen[Int] = Gen.union(gen1, gen19)
    val (_, overallList) = (1 to 100).foldLeft((rng, List.empty[Int])) { (tuple, _) =>
      val (r, listSoFar) = tuple
      val (i, newRng) = gen.sample.run(r)
      println(i)
      (newRng, listSoFar :+ i)
    }
    overallList.forall(x => x == 1 || x == 19) must beTrue
  }
}
