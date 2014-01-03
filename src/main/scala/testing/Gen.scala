package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.state.{ State, RNG }

trait Prop {
  import Prop._
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    def check = this.check match {
      case Right(successCount) => p.check match {
        case Right(otherSuccessCount) =>
          Right(successCount + otherSuccessCount)
        case Left((failedCase, otherSuccessCount)) =>
          Left((failedCase, successCount + otherSuccessCount))
      }
      case Left((failedCase, successCount)) => p.check match {
        case Right(otherSuccessCount) =>
          Left((failedCase, successCount + otherSuccessCount))
        case Left((otherFailedCase, otherSuccessCount)) =>
          Left((failedCase + ", " + otherFailedCase, successCount + otherSuccessCount))
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[A](sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int) = {
    val rng = RNG.simple(1234L)
    val range: Int = stopExclusive - start
    val state = rng.positiveLessThan(range).map(n => n + start)
    Gen(state)
  }

  def listOf[A](a: Gen[A]): Gen[List[A]] =
    new Gen(null)
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    new Gen(null)
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    new Prop {
      def check = Right(0)
    }
}
