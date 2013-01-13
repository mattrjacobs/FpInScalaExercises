package com.mattrjacobs.fp

import org.specs2._

class Exercise1 extends Specification {
  def is =
    "Exercise1" ^
      "wider_1" ! wider_1 ^
      "taller_1" ! taller_1 ^
      "wider_2" ! wider_2 ^
      "taller_2" ! taller_2 ^
      "wider" ! wider ^
      "taller" ! taller

  val box1 = Box(1.0, 2.0)
  val box2 = Box(3.0, 1.0)

  def wider_1 = Box.wider_1(box1, box2) must_== box1
  def wider_2 = Box.wider_2(box1, box2) must_== box1
  def wider = Box.wider(box1, box2) must_== box1

  def taller_1 = Box.taller_1(box1, box2) must_== box2
  def taller_2 = Box.taller_2(box1, box2) must_== box2
  def taller = Box.taller(box1, box2) must_== box2
}
