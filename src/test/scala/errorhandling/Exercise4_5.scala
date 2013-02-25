package com.mattrjacobs.fp

import org.specs2._

class Exercise4_5 extends Specification {
  def is =
    "Exercise 4-5" ^
      "Option.sequence" ^
      "Option.sequence empty" ! emptySequence ^
      "Option.sequence all Nones" ! nonesSequence ^
      "Option.sequence some Nones" ! someNonesSequence ^
      "Option.sequence all Somes" ! somesSequence ^
      p ^
      "Option.sequence_in_terms_of_traverse" ^
      "Option.sequence empty" ! emptySequenceT ^
      "Option.sequence all Nones" ! nonesSequenceT ^
      "Option.sequence some Nones" ! someNonesSequenceT ^
      "Option.sequence all Somes" ! somesSequenceT

  def emptySequence = Option.sequence(Nil) must_== Some(Nil)
  def nonesSequence = Option.sequence(List(None, None, None)) must_== None
  def someNonesSequence = Option.sequence(List(Some("x"), None, Some("l"))) must_== None
  def somesSequence = Option.sequence(List(Some(1), Some(2), Some(3))) must_== Some(List(1, 2, 3))
  def emptySequenceT = Option.sequence_in_terms_of_traverse(Nil) must_== Some(Nil)
  def nonesSequenceT = Option.sequence_in_terms_of_traverse(List(None, None, None)) must_== None
  def someNonesSequenceT = Option.sequence_in_terms_of_traverse(List(Some("x"), None, Some("l"))) must_== None
  def somesSequenceT = Option.sequence_in_terms_of_traverse(List(Some(1), Some(2), Some(3))) must_== Some(List(1, 2, 3))
}
