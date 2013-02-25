package com.mattrjacobs.fp

import org.specs2._

class Exercise4_8 extends Specification {
  def is =
    "Exercise 4-8" ^
      "Either.sequence" ^
      "Either.sequence empty" ! emptySequence ^
      "Either.sequence all Lefts" ! leftsSequence ^
      "Either.sequence some Rights" ! someRightsSequence ^
      "Either.sequence all Rights" ! rightsSequence ^
      p ^
      "Either.traverse" ^
      "Either.traverse empty" ! emptyTraverse ^
      "Either.traverse all Lefts" ! leftsTraverse ^
      "Either.traverse some Right" ! someRightsTraverse ^
      "Either.traverse all Rights" ! rightsTraverse

  def rightEven(x: Int): Either[String, Int] = x match {
    case x if x % 2 == 0 => Right(x)
    case x               => Left(x.toString + " is odd")
  }

  def emptySequence = Either.sequence(Nil) must_== Right(Nil)
  def leftsSequence = Either.sequence(List(Left("a"), Left("b"))) must_== Left("a")
  def someRightsSequence = Either.sequence(List(Right(1), Left("b"))) must_== Left("b")
  def rightsSequence = Either.sequence(List(Right(1), Right(2))) must_== Right(List(1, 2))

  def emptyTraverse = Either.traverse(Nil)(rightEven) must_== Right(Nil)
  def leftsTraverse = Either.traverse(List(1, 3, 5))(rightEven) must_== Left("1 is odd")
  def someRightsTraverse = Either.traverse(List(2, 3, 4))(rightEven) must_== Left("3 is odd")
  def rightsTraverse = Either.traverse(List(2, 4, 6))(rightEven) must_== Right(List(2, 4, 6))
}
