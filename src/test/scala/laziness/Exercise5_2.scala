package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_2 extends Specification {
  import Stream._
  def one = {
    throw new RuntimeException("Shouldn't evaluate this!")
    1
  }

  val stream = cons(1, cons(2, cons(3, cons(4, Stream.empty))))
  val explodingStream = cons(one, cons(2, Stream.empty))

  def is =
    "Exercise 5-2" ^
      "Stream.take" ^
      "0" ! takeNone ^
      "2" ! takeTwo ^
      "4" ! takeAll
  //"shouldn't evaluate args" ! takeFromExplodingStream ^
  //"should evaluate in combination with toList" ! takeFromExplodingStreamThenToList

  def takeNone = stream.take(0).toList must_== Stream.empty.toList

  def takeTwo = stream.take(2).toList must_== List(1, 2)

  def takeAll = stream.take(4).toList must_== List(1, 2, 3, 4)

  //def takeFromExplodingStream = explodingStream.take(2) must not beNull

  /*def takeFromExplodingStreamThenToList =
    try {
      explodingStream.take(2).toList must not beNull
    } catch {
      case ex: RuntimeException => ex must not beNull
    }*/
}
