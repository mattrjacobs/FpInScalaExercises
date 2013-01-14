package com.mattrjacobs.fp

import org.specs2._

class Exercise5 extends Specification {
  def is =
    "Exercise5" ^
      "even" ^
      "yes" ! evenYes ^
      "no" ! evenNo

  def evenYes = Operation.even(2) must beTrue
  def evenNo = Operation.even(1) must beFalse
}
