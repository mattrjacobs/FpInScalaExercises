package com.mattrjacobs.fp.state

trait RNG {
  def nextInt: (Int, RNG)

  def randomPair: ((Int, Int), RNG) = {
    val (i1, rng2) = nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }
}

object RNG {
  type Rand[A] = State[RNG, A]

  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  val int: Rand[Int] =
    State(rng => rng.nextInt)

  def unit[A](a: A): Rand[A] =
    State(rng => (a, rng))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    State(rng => {
      val (a, rng2) = f.run(rng)
      g(a).run(rng2)
    })

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(f(a, _)))
  }

  def positiveLessThan(n: Int): Rand[Int] =
    flatMap(positiveInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod > 0) unit(mod) else positiveLessThan(n)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def positiveEven: Rand[Int] =
    map(positiveInt)(i => i - i % 2)

  def positiveInt: Rand[Int] =
    State(rng => {
      val (i, rng2) = rng.nextInt
      i match {
        case p if p > 0   => (p, rng2)
        case Int.MinValue => positiveInt.run(rng2)
        case 0            => positiveInt.run(rng2)
        case n if n < 0   => (-n, rng2)
      }
    })

  def double: Rand[Double] = doubleViaMap

  def doubleViaMap: Rand[Double] =
    map(positiveInt)(i => (i / (Int.MaxValue.toDouble + 1)))

  def boolean: Rand[Boolean] =
    map(positiveInt)(i => i % 2 == 0)

  def doubleDirect: Rand[Double] =
    State(rng => {
      val (i, rng2) = positiveInt.run(rng)
      (i / (Int.MaxValue.toDouble + 1), rng2)
    })

  def intDouble: Rand[(Int, Double)] = intDoubleViaMap2

  def intDoubleViaMap2: Rand[(Int, Double)] = both(int, double)

  def intDoubleDirect: Rand[(Int, Double)] =
    State(rng => {
      val (i, rng2) = rng.nextInt
      val (d, rng3) = double.run(rng2)
      ((i, d), rng3)
    })

  def doubleInt: Rand[(Double, Int)] = doubleIntViaMap2

  def doubleIntViaMap2: Rand[(Double, Int)] = both(double, int)

  def doubleIntDirect: Rand[(Double, Int)] =
    State(rng => {
      val ((i, d), rng2) = intDouble.run(rng)
      ((d, i), rng2)
    })

  def double3: Rand[(Double, Double, Double)] =
    State(rng => {
      val (d1, rng2) = double.run(rng)
      val (d2, rng3) = double.run(rng2)
      val (d3, rng4) = double.run(rng3)
      ((d1, d2, d3), rng4)
    })

  def ints(count: Int): Rand[List[Int]] = intsViaSequence(count)

  def intsViaSequence(count: Int): Rand[List[Int]] =
    RNG.sequence(List.fill(count)(int))

  def intsDirect(count: Int): Rand[List[Int]] =
    State((rng: RNG) => {
      (1 to count).foldRight((Nil: List[Int], rng)) {
        case (_, (l, r)) => r.nextInt match {
          case (next, nextRng) => (l :+ next, nextRng)
        }
      }
    })

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    State(rng => fs.foldRight((Nil: List[A], rng)) {
      case (f, (l, r)) => {
        val (nextValue, nextRng) = f.run(r)
        (l :+ nextValue, nextRng)
      }
    })
}
