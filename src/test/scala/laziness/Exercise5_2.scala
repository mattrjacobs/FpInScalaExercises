package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_2 extends Specification {
  import Stream._
  def three = {
    throw new RuntimeException("Shouldn't evaluate this!")
    3
  }

  lazy val stream = cons(1, cons(2, cons(3, cons(4, Stream.empty))))
  lazy val explodingStream = cons(1, cons(2, cons(three, Stream.empty)))

  def is =
    "Exercise 5-2" ^
      "Stream.take" ^
      "0" ! takeNone ^
      "2" ! takeTwo ^
      "4" ! takeAll ^
      "shouldn't evaluate args" ! takeFromExplodingStream ^
      "should evaluate in combination with toList" ! takeFromExplodingStreamThenToList

  def takeNone = stream.take(0).toList must_== Stream.empty.toList

  def takeTwo = stream.take(2).toList must_== List(1, 2)

  def takeAll = stream.take(4).toList must_== List(1, 2, 3, 4)

  def takeFromExplodingStream = explodingStream.take(2) must not beNull

  def takeFromExplodingStreamThenToList =
    explodingStream.take(3).toList must throwA[RuntimeException]
}
