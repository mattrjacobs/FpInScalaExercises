package com.mattrjacobs.fp

import org.specs2._

class Exercise13 extends Specification {
  def is =
    "Exercise13" ^
      "Newton's Method - square root" ^
      "2" ! sqrt2 ^
      "4" ! sqrt4 ^
      "10" ! sqrt10

  def sqrt2 = Recursive.sqrt(2) must beCloseTo(1.414, 0.1)
  def sqrt4 = Recursive.sqrt(4) must beCloseTo(2, 0.0000001)
  def sqrt10 = Recursive.sqrt(10) must beCloseTo(3.16227766017, 0.0000001)
}
