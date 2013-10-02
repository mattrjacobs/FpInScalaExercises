package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_5 extends Specification {
  import Stream._

  val stream = cons(1, cons(2, cons(3, cons(4, Stream.empty))))

  lazy val grenade: Int = {
    throw new RuntimeException("grenade")
    3
  }

  val explodingStream = cons(1, cons(2, cons(grenade, cons(4, Stream.empty))))

  def is =
    "Exercise 5-5 (takeWhile using foldRight)" ^
      "Stream.takeWhile" ^
      "true" ! takeAll ^
      "up to 2" ! takeUpTo2 ^
      "up to 3" ! takeUpTo3 ^
      "doesn't get to exploding" ! takeNotExploding ^
      "does get to exploding" ! takeExploding

  def takeAll = stream.takeWhileViaFoldRight(_ => true).take(4).toList must_== List(1, 2, 3, 4)

  def takeUpTo2 = stream.takeWhileViaFoldRight(n => n < 3).toList must_== List(1, 2)

  def takeUpTo3 = stream.takeWhileViaFoldRight(n => n < 4).toList must_== List(1, 2, 3)

  def takeNotExploding = explodingStream.takeWhileViaFoldRight(n => n < 2).toList must_== List(1)

  def takeExploding = explodingStream.takeWhileViaFoldRight(n => n < 4).toList must throwA[RuntimeException]
}
