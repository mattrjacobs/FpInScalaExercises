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

  val divBy10 = Operation.divisibleBy(10)
  val divBy40 = Operation.divisibleBy(40)

  def divisibleTenYes = divBy10(90) must beTrue
  def divisibleTenNo = divBy10(88) must beFalse
  def divisibleFortyYes = divBy40(120) must beTrue
  def divisibleFortyNo = divBy40(10) must beFalse
}
