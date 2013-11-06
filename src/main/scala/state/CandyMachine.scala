package com.mattrjacobs.fp.state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.unit((candies, coins))
}

object CandyMachine {
  def main(args: Array[String]) = {
    println("Starting up Candy machine!")

    val m = Machine(true, 15, 2)
    val inputs = Nil
    val state = m.simulateMachine(inputs)
  }
}
