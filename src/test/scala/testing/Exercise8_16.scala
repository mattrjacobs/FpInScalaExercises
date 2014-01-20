package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.parallelism.Par
import com.mattrjacobs.fp.state.RNG
import java.util.concurrent.Executors
import org.specs2._

class Exercise8_16 extends Specification {

  val rng = RNG.simple(11234L)
  val InitialPar = Executors.newCachedThreadPool

  def is =
    "Exercise 8-16" ^
      "Par testing" ^
      "first" ! first ^
      "second" ! second ^
      "third" ! third ^
      "fourth" ! fourth

  def first = {
    val p = Prop.forAll(Gen.unit(Par.unit(1))) { i =>
      Par.map(i)(_ + 1)(InitialPar).get == Par.unit(2)(InitialPar).get
    }

    Prop.run(p) mustEqual None
  }

  def second = {
    val p2 = Prop.check {
      val par1 = Par.map(Par.unit(1))(_ + 1)
      val par2 = Par.unit(2)
      par1(InitialPar).get == par2(InitialPar).get
    }

    Prop.run(p2) mustEqual None
  }

  def third = {
    val p3 = Prop.check {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2))(InitialPar).get
    }

    Prop.run(p3) mustEqual None
  }

  def fourth = {
    val p4 = Prop.checkPar {
      Par.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2))
    }

    Prop.run(p4) mustEqual None
  }
}
