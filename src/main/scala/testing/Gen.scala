package com.mattrjacobs.fp.testing

import com.mattrjacobs.fp.laziness.Stream
import com.mattrjacobs.fp.state.{ State, RNG }

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g2: Gen[B])(f: (A, B) => C) =
    Gen.map2(this, g2)(f)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
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

  def listOf[A](a: Gen[A]): SGen[List[A]] =
    SGen(num => listOfN(num, a))

  def listOf1[A](a: Gen[A]): SGen[List[A]] =
    SGen(num => listOfN(num + 1, a))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean().flatMap {
      case true  => g1
      case false => g2
    }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val BIG = 10000
    val totalWeight = g1._2 + g2._2
    val simulatedMax = (totalWeight * BIG).toInt
    val threshold = (g1._2 * BIG).toInt
    Gen.choose(1, simulatedMax + 1).flatMap {
      case u if u <= threshold => g1._1
      case o                   => g2._1
    }
  }

  def unsized[A](a: Gen[A]): SGen[A] = SGen(_ => a)

  def map2[A, B, C](g1: Gen[A], g2: Gen[B])(f: (A, B) => C): Gen[C] =
    for {
      a1 <- g1
      a2 <- g2
    } yield f(a1, a2)

  val weightedExecutor = {
    import java.util.concurrent.Executors
    weighted(
      Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
      Gen.unit(Executors.newCachedThreadPool) -> 0.25)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}
