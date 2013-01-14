package com.mattrjacobs.fp

import org.specs2._

class Exercise4 extends Specification {
  def is =
    "Exercise4" ^
      "divisibleBy" ^
      "10" ^
      "yes" ! divisibleTenYes ^
      "no" ! divisibleTenNo ^
      p ^
      "40" ^
      "yes" ! divisibleFortyYes ^
      "no" ! divisibleFortyNo

  def divisibleTenYes = Operation.divisibleBy(10)(5) must beTrue
  def divisibleTenNo = Operation.divisibleBy(10)(4) must beFalse
  def divisibleFortyYes = Operation.divisibleBy(40)(8) must beTrue
  def divisibleFortyNo = Operation.divisibleBy(40)(9) must beFalse
}
