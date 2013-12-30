package com.mattrjacobs.fp.parallelism

import java.util.concurrent.ScheduledThreadPoolExecutor
import org.specs2._

class Exercise7_11 extends Specification {

  val choices = List(Par.unit("a"), Par.unit("b"), Par.unit("c"))

  val executor = new ScheduledThreadPoolExecutor(4)

  def is =
    "Exercise 7-11" ^
      "choiceN" ^
      "returns the first computation when chooser is 0" ! choice0 ^
      "returns the third computation when chooser is 2" ! choice2

  def choice0 = {
    val chooser = Par.unit(0)
    Par.run(executor)(Par.choiceN(chooser)(choices)).get mustEqual ("a")
  }

  def choice2 = {
    val chooser = Par.unit(2)
    Par.run(executor)(Par.choiceN(chooser)(choices)).get mustEqual ("c")
  }
}
