package com.mattrjacobs.fp.state

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulateMachine(inputs: List[Input]): State[Machine, Int] =
    State({ inputMachine =>
      inputs.foldLeft((inputMachine.coins, inputMachine)) {
        case ((c, m), Coin) if m.locked && m.candies > 0 => {
          val newMachine = m.copy(locked = false, coins = m.coins + 1)
          (c + 1, newMachine)
        }
        case ((c, m), Turn) if !m.locked && m.candies > 0 => {
          val newMachine = m.copy(locked = true, candies = m.candies - 1)
          (c, newMachine)
        }
        case ((c, m), Coin) if !m.locked => {
          val newMachine = m.copy(coins = m.coins + 1)
          (c + 1, newMachine)
        }
        case ((c, m), i) => (c, m)
      }
    })
}
