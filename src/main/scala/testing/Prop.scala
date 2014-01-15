package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.laziness.Stream
import com.mattrjacobs.fp.state.RNG
import Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p2: Prop) = Prop { (max, num, rng) =>
    run(max, num, rng) orElse p2.run(max, num, rng)
  }

  def ||(p2: Prop) = Prop { (max, num, rng) =>
    for {
      firstResult <- run(max, num, rng)
      secondResult <- p2.run(max, num, rng)
    } yield (max, firstResult._2 + secondResult._2, num)
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type Result = Option[(MaxSize, FailedCase, TestCases)]

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace: " + "\n" +
      e.getStackTrace.mkString("\n")

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).foldRight(None: Result) {
      case ((a, i), r) => try {
        if (f(a)) r else Some((max, a.toString, i))
      } catch { case e: Exception => Some((max, buildMsg(a, e), i)) }
    }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = {
    forAll(g(_))(f)
  }
}

