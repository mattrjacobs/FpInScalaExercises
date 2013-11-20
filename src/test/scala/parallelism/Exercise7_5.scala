package com.mattrjacobs.fp.parallelism

import java.util.concurrent.ScheduledThreadPoolExecutor
import org.specs2._

class Exercise7_5 extends Specification {

  def f(i: Int) = i + 4
  def id(i: Int) = i

  val executor = new ScheduledThreadPoolExecutor(4)

  def is =
    "Exercise 7-5" ^
      "parMap" ^
      "returns the same as map for empty list" ! parMapEmpty ^
      "returns the same as map for non-Nil identity" ! parMapIdentity ^
      "returns the same as map for non-Nil transformation" ! parMapTransformation

  def parMapEmpty = {
    val seq = Nil.map(f)
    val parallel = Par.parMap(Nil)(f)(executor).get()
    seq == parallel
  }

  def parMapIdentity = {
    val seq = List(1, 2, 3).map(id)
    val parallel = Par.parMap(List(1, 2, 3))(id)(executor).get()
    seq == parallel
  }

  def parMapTransformation = {
    val seq = List(1, 2, 3).map(f)
    val parallel = Par.parMap(List(1, 2, 3))(f)(executor).get()
    seq == parallel
  }
}
