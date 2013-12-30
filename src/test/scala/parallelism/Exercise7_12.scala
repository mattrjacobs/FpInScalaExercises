package com.mattrjacobs.fp.parallelism

import java.util.concurrent.ScheduledThreadPoolExecutor
import org.specs2._

class Exercise7_12 extends Specification {

  val choices = Map("z" -> Par.unit("a"),
    "x" -> Par.unit("b"),
    "u" -> Par.unit("c"))

  val executor = new ScheduledThreadPoolExecutor(4)

  def is =
    "Exercise 7-11" ^
      "choiceN" ^
      "returns the first computation when chooser is z" ! choiceZ ^
      "returns the second computation when chooser is x" ! choiceX ^
      "returns the third computation when chooser is u" ! choiceU

  def choiceZ = {
    val chooser = Par.unit("z")
    Par.run(executor)(Par.choiceMap(chooser)(choices)).get mustEqual ("a")
  }

  def choiceX = {
    val chooser = Par.unit("x")
    Par.run(executor)(Par.choiceMap(chooser)(choices)).get mustEqual ("b")
  }

  def choiceU = {
    val chooser = Par.unit("u")
    Par.run(executor)(Par.choiceMap(chooser)(choices)).get mustEqual ("c")
  }

}
