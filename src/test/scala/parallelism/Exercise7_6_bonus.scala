package com.mattrjacobs.fp.parallelism

import java.util.concurrent.ScheduledThreadPoolExecutor
import org.specs2._

class Exercise7_6_Bonus extends Specification {

  val executor = new ScheduledThreadPoolExecutor(4)

  def is =
    "Exercise 7-6 (Bonus)" ^
      "parCount" ^
      "returns 0 for empty List" ! emptyCount ^
      "returns 5 for List(foo,foo,foo,foo,foo)" ! fiveCount ^
      "returns 20 for List(foo bar x 10)" ! twentyCount

  def emptyCount = {
    val parallel = Par.parCount(Nil)
    parallel(executor).get() == 0
  }

  def fiveCount = {
    val parallel = Par.parCount(List("foo", "foo", "foo", "foo", "foo"))
    parallel(executor).get() == 5
  }

  def twentyCount = {
    val l: List[String] = List.fill(10)("foo bar")
    val parallel = Par.parCount(l)
    parallel(executor).get() == 20
  }
}
