package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.laziness.Stream
import com.mattrjacobs.fp.state.RNG
import Prop._

case class Prop(run: (TestCases, RNG) => Result)

object Prop {
  type TestCases = Int
  type FailedCase = String
  type Result = Option[(FailedCase, TestCases)]

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace: " + "\n" +
      e.getStackTrace.mkString("\n")
}

