package com.mattrjacobs.fp.state

trait RNG {
  def nextInt: (Int, RNG)
  def positiveInt(rng: RNG): (Int, RNG)
  def double(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
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

    def double(rng: RNG): (Double, RNG) = {
      val (i, rng2) = positiveInt(rng)
      (i / (Int.MaxValue.toDouble + 1), rng2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng2) = nextInt
      val (d, rng3) = double(rng2)
      ((i, d), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng2) = double(rng)
      val (d2, rng3) = double(rng2)
      val (d3, rng4) = double(rng3)
      ((d1, d2, d3), rng4)
    }
  }
}
