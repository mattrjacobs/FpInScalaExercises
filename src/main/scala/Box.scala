package com.mattrjacobs.fp

case class Box(height: Double, width: Double)

object Box {
  def wider_1(x: Box, y: Box): Box =
    if (x.width > y.width) x else y

  def taller_1(x: Box, y: Box): Box =
    if (x.height > y.height) x else y

  def wider_2(x: Box, y: Box): Box =
    greaterBy(x, y, p => p.width)

  def taller_2(x: Box, y: Box): Box =
    greaterBy(x, y, p => p.height)

  private def greaterBy(x: Box, y: Box, f: Box => Double): Box =
    if (f(x) > f(y)) x else y

  /**
   * EXERCISE 1: Use the underscore notation to get a nicer-looking definition
   * of taller and wider
   */

  val wider: (Box, Box) => Box = greaterBy(_, _, p => p.width)
  val taller: (Box, Box) => Box = greaterBy(_, _, p => p.height)
}
