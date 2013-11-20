package com.mattrjacobs.fp.parallelism

import java.util.concurrent.ScheduledThreadPoolExecutor
import org.specs2._

class Exercise7_6 extends Specification {

  def isEven(i: Int) = i % 2 == 0

  val executor = new ScheduledThreadPoolExecutor(4)

  def is =
    "Exercise 7-6" ^
      "parFilter" ^
      "returns the same as map for empty list" ! parFilterEmpty ^
      "returns the same as map for List(1-10)" ! parFilterWorks

  def parFilterEmpty = {
    Par.parFilter(Nil)(isEven)(executor).get() == Nil
  }

  def parFilterWorks = {
    Par.parFilter((1 to 10).toList)(isEven)(executor).get() == List(2, 4, 6, 8, 10)
  }
}
