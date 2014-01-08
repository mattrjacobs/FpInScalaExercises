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

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]) =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(count: Gen[Int]): Gen[List[A]] =
    count.flatMap(c => Gen.listOfN(c, this))
}

object Gen {
  def choose(start: Int, stopExclusive: Int) =
    Gen(RNG.positiveInt.map(n => (n % (stopExclusive - start)) + start))

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    Gen.choose(start, stopExclusive).map {
      case e if e % 2 == 0               => e
      case o if o % 2 == 1 && o > start  => o - 1
      case o if o % 2 == 1 && o == start => o + 1
    }

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    Gen.choose(start, stopExclusive).map {
      case e if e % 2 == 0 && e > start  => e - 1
      case e if e % 2 == 0 && e == start => e + 1
      case o if o % 2 == 1               => o
    }

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean(): Gen[Boolean] =
    Gen(RNG.boolean)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
    Gen.choose(from, to).flatMap {
      case evenInitial if evenInitial % 2 == 0 =>
        even(from, to).map(b => (evenInitial, b))
      case oddInitial if oddInitial % 2 == 1 =>
        odd(from, to).map(b => (oddInitial, b))
    }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](a: Gen[A]): Gen[List[A]] =
    new Gen(null)

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    new Prop {
      def check = Right(0)
    }
}
