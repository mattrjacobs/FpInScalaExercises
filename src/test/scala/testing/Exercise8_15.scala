package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.RNG
import org.specs2._

class Exercise8_15 extends Specification {

  val rng = RNG.simple(11234L)

  def is =
    "Exercise 8-15" ^
      "List.sorted" ^
      "has same length" ! sameLength ^
      "has all elements in right order, pair-wise" ! pairWiseOrdered

  def sameLength = {
    val lengthProp = Prop.forAll(Gen.listOf(Gen.choose(-1000, 1000))) { l =>
      l.size == l.sorted.size
    }

    Prop.run(lengthProp) mustEqual None
  }

  def pairWiseOrdered = {
    val p = Prop.forAll(Gen.listOf(Gen.choose(-1000, 1000))) { l =>
      val windows = l.sorted.sliding(2)
      windows.foldLeft(true) { (initialBool, elements) =>
        initialBool && (elements match {
          case Nil              => true
          case _ :: Nil         => true
          case e1 :: e2 :: rest => e1 <= e2
        })
      }
    }

    Prop.run(p) mustEqual None
  }
}
