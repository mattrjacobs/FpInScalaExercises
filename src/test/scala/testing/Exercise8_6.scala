package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class Exercise8_6 extends Specification {

  val rng = RNG.simple(1234L)

  def is =
    "Exercise 8-6" ^
      "Gen.flatMap" ^
      "works" ! flatMap ^
      p ^
      "Gen.sameParity" ^
      "one possibility" ! constrainedSameParity ^
      "many possibilities" ! multiSameParity ^
      p ^
      "Gen.listOfN" ^
      "one possibility" ! constrainedListOfN ^
      "many possibilities" ! multiListOfN

  def flatMap = {
    val gen: Gen[Int] = Gen.choose(2, 4)
    val f: (Int => Gen[Int]) = (x: Int) => Gen.choose(10, x + 10)
    val genB: Gen[Int] = gen.flatMap(f)

    val (_, overallList) = (1 to 100).foldLeft((rng, List.empty[Int])) { (tuple, _) =>
      val (r, listSoFar) = tuple
      val (i, newRng) = genB.sample.run(r)
      (newRng, listSoFar :+ i)
    }
    val count = overallList.groupBy(k => k)
    (count(10).size > count(12).size) && (count(11).size > count(12).size) must beTrue
  }

  def constrainedSameParity = {
    val gen = Gen.sameParity(1, 3)

    val (_, overallList) = (1 to 100).foldLeft((rng, List.empty[(Int, Int)])) { (tuple, _) =>
      val (r, listSoFar) = tuple
      val ((a, b), newRng) = gen.sample.run(r)
      (newRng, listSoFar :+ (a, b))
    }
    overallList.forall {
      case (a, b) => a == b
    } must beTrue
  }

  def multiSameParity = {
    val gen = Gen.sameParity(0, 100)

    val (_, overallList) = (1 to 100).foldLeft((rng, List.empty[(Int, Int)])) {
      (tuple, _) =>
        val (r, listSoFar) = tuple
        val ((a, b), newRng) = gen.sample.run(r)
        (newRng, listSoFar :+ (a, b))
    }
    overallList.forall {
      case (a, b) => (a - b) % 2 == 0
    } must beTrue
  }

  def constrainedListOfN = {
    val countGen = Gen.choose(6, 7) //must choose 6
    val gen = Gen.choose(0, 100).listOfN(countGen)

    val (_, overallList) = (1 to 100).foldLeft((rng, List.empty[List[Int]])) {
      (tuple, _) =>
        val (r, listSoFar) = tuple
        val (l, newRng) = gen.sample.run(r)
        (newRng, listSoFar :+ l)
    }
    overallList.forall(l =>
      l.size == 6 && l.forall(i =>
        i >= 0 && i < 100)) must beTrue
  }

  def multiListOfN = {
    val countGen = Gen.choose(0, 100)
    val gen = Gen.choose(0, 100).listOfN(countGen)

    val (_, overallList) = (1 to 100).foldLeft((rng, List.empty[List[Int]])) {
      (tuple, _) =>
        val (r, listSoFar) = tuple
        val (l, newRng) = gen.sample.run(r)
        (newRng, listSoFar :+ l)
    }
    overallList.forall(l =>
      l.size >= 0 && l.size < 100 && l.forall(i =>
        i >= 0 && i < 100)) must beTrue
  }
}
