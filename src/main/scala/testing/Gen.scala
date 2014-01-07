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
  def choose(start: Int, stopExclusive: Int) =
    Gen(RNG.positiveInt.map(n => (n % (stopExclusive - start)) + start))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean(): Gen[Boolean] =
    Gen(RNG.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](a: Gen[A]): Gen[List[A]] =
    new Gen(null)

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    new Prop {
      def check = Right(0)
    }
}
