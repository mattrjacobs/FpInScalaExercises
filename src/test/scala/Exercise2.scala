package com.mattrjacobs.fp

import org.specs2._

class Exercise2 extends Specification {
  def is =
    "Exercise2" ^
      "even" ^
      "yes" ! evenYes ^
      "no" ! evenNo ^
      p ^
      "negative" ^
      "yes" ! negativeYes ^
      "no" ! negativeNo ^
      p ^
      "odd_1" ^
      "yes" ! odd_1Yes ^
      "no" ! odd_1No ^
      p ^
      "positive_1" ^
      "yes" ! positive_1Yes ^
      "no" ! positive_1No ^
      p ^
      "odd" ^
      "yes" ! oddYes ^
      "no" ! oddNo ^
      p ^
      "positive" ^
      "yes" ! positiveYes ^
      "no" ! positiveNo ^
      p ^
      "absoluteMonomorphic" ^
      "cube" ^
      "positive" ! cubePositive ^
      "negative" ! cubeNegative

  def evenYes = Operation.even(2) must beTrue
  def evenNo = Operation.even(1) must beFalse
  def negativeYes = Operation.negative(-9) must beTrue
  def negativeNo = Operation.negative(6) must beFalse
  def odd_1Yes = Operation.odd_1(5) must beTrue
  def odd_1No = Operation.odd_1(4) must beFalse
  def positive_1Yes = Operation.positive_1(5) must beTrue
  def positive_1No = Operation.positive_1(-4) must beFalse
  def oddYes = Operation.odd(3) must beTrue
  def oddNo = Operation.odd(10) must beFalse
  def positiveYes = Operation.positive(5) must beTrue
  def positiveNo = Operation.positive(-9) must beFalse
  def cubePositive = Operation.absoluteMonomorphic(cube)(3) mustEqual 27
  def cubeNegative = Operation.absoluteMonomorphic(cube)(-3) mustEqual 27

  private def cube(n: Int) = n * n * n

}
