package com.mattrjacobs.fp

import org.specs2._

class Exercise3 extends Specification {
  def is =
    "Exercise3" ^
      "absolute" ^
      "cube" ^
      "positive" ! cubePositive ^
      "negative" ! cubeNegative

  def cubePositive = Operation.absolute(cube)(3) mustEqual 27
  def cubeNegative = Operation.absolute(cube)(-3) mustEqual 27

  private def cube(n: Int): Int = n * n * n

}
