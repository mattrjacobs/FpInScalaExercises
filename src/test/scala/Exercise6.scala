package com.mattrjacobs.fp

import org.specs2._
import Operation._

class Exercise6 extends Specification {
  def is =
    "Exercise6" ^
      "divisibleBy3And5" ^
      "simple" ^
      "both" ! divSimpleAndBoth ^
      "left" ! divSimpleAndLeft ^
      "right" ! divSimpleAndRight ^
      "neither" ! divSimpleAndNeither ^
      p ^
      "lifted" ^
      "both" ! divLiftedAndBoth ^
      "left" ! divLiftedAndLeft ^
      "right" ! divLiftedAndRight ^
      "neither" ! divLiftedAndNeither ^
      p ^
      p ^
      "divisibleBy3Or5" ^
      "simple" ^
      "both" ! divSimpleOrBoth ^
      "left" ! divSimpleOrLeft ^
      "right" ! divSimpleOrRight ^
      "neither" ! divSimpleOrNeither ^
      p ^
      "lifted" ^
      "both" ! divLiftedOrBoth ^
      "left" ! divLiftedOrLeft ^
      "right" ! divLiftedOrRight ^
      "neither" ! divLiftedOrNeither

  val divAndBoth =
    (f: Pred[Int]) => f(30) must beTrue
  val divAndLeft =
    (f: Pred[Int]) => f(27) must beFalse
  val divAndRight =
    (f: Pred[Int]) => f(20) must beFalse
  val divAndNeither =
    (f: Pred[Int]) => f(44) must beFalse
  val divOrBoth =
    (f: Pred[Int]) => f(30) must beTrue
  val divOrLeft =
    (f: Pred[Int]) => f(27) must beTrue
  val divOrRight =
    (f: Pred[Int]) => f(20) must beTrue
  val divOrNeither =
    (f: Pred[Int]) => f(44) must beFalse

  val simpleAnd = Operation.divisibleBy3And5Simple
  def divSimpleAndBoth = divAndBoth(simpleAnd)
  def divSimpleAndLeft = divAndLeft(simpleAnd)
  def divSimpleAndRight = divAndRight(simpleAnd)
  def divSimpleAndNeither = divAndNeither(simpleAnd)

  val liftedAnd = Operation.divisibleBy3And5
  def divLiftedAndBoth = divAndBoth(liftedAnd)
  def divLiftedAndLeft = divAndLeft(liftedAnd)
  def divLiftedAndRight = divAndRight(liftedAnd)
  def divLiftedAndNeither = divAndNeither(liftedAnd)

  val simpleOr = Operation.divisibleBy3Or5Simple
  def divSimpleOrBoth = divOrBoth(simpleOr)
  def divSimpleOrLeft = divOrLeft(simpleOr)
  def divSimpleOrRight = divOrRight(simpleOr)
  def divSimpleOrNeither = divOrNeither(simpleOr)

  val liftedOr = Operation.divisibleBy3Or5
  def divLiftedOrBoth = divOrBoth(liftedOr)
  def divLiftedOrLeft = divOrLeft(liftedOr)
  def divLiftedOrRight = divOrRight(liftedOr)
  def divLiftedOrNeither = divOrNeither(liftedOr)
}
