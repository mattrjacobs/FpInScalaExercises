package com.mattrjacobs.fp.state

trait RNG {
  def nextInt: (Int, RNG)
  def positiveInt(rng: RNG): (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }

    def randomPair: ((Int, Int), RNG) = {
      val (i1, rng2) = nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    def positiveInt(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt
      i match {
        case p if p > 0   => (p, rng2)
        case Int.MinValue => positiveInt(rng2)
        case 0            => positiveInt(rng2)
        case n if n < 0   => (-n, rng2)
      }
    }

  }
}
