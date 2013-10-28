package com.mattrjacobs.fp.state

import org.specs2._

trait TestRng extends Specification {
  val rng = RNG.simple(12345L)

  protected def doubleOk(d: Double) = {
    d must beLessThanOrEqualTo(1.0)
    d must beGreaterThanOrEqualTo(0.0)
  }
}
