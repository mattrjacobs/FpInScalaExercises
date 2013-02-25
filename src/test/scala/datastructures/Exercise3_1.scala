package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_1 extends Specification {
  def is =
    "Exercise3-1" ^
      "List.x" ! x

  def x = List.x must_== 3
}
