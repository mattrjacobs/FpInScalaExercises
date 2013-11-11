package com.mattrjacobs.fp.state

import org.specs2._

class Exercise6_11 extends TestRng {
  def is =
    "Exercise 6-11" ^
      "Candy Machine FSM" ^
      "returns initial state with no input" ! noInput ^
      "from (L, 0, 0)" ^
      "insert a coin is ignored" ! coinOnL00Stays ^
      "turn the knob is ignored" ! turnOnL00Stays ^
      p ^
      "from (L, 1, 0)" ^
      "insert a coin transitions to (U, 1, 1)" ! coinOnL10TransitionsToU11 ^
      "turn the knob is ignored" ! turnOnL10Stays ^
      p ^
      "from (U, 1, 1)" ^
      "insert a coin into an unlocked machine transitions to (U, 1, 2)" ! coinOnU11TransitionsToU12 ^
      "turn the knob transitions to (L, 0, 1)" ! turnOnU11TransitionsToL01 ^
      p ^
      "from (L, 10, 10)" ^
      "Seq(Coin, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Coin, Turn) transitions to (L, 6, 15)" ! machineSeq

  def noInput = {
    val m = Machine(false, 15, 2)
    val s = m.simulateMachine(Nil)
    s.run(m) == (2, m)
  }

  def coinOnL00Stays = {
    val m = Machine(true, 0, 0)
    val s = m.simulateMachine(List(Coin))
    s.run(m) == (0, m)
  }

  def turnOnL00Stays = {
    val m = Machine(true, 0, 0)
    val s = m.simulateMachine(List(Turn))
    s.run(m) == (0, m)
  }

  def coinOnL10TransitionsToU11 = {
    val m = Machine(true, 1, 0)
    val s = m.simulateMachine(List(Coin))
    s.run(m) == (1, Machine(false, 1, 1))
  }

  def turnOnL10Stays = {
    val m = Machine(true, 1, 0)
    val s = m.simulateMachine(List(Turn))
    s.run(m) == (0, m)
  }

  def coinOnU11TransitionsToU12 = {
    val m = Machine(false, 1, 1)
    val s = m.simulateMachine(List(Coin))
    s.run(m) == (2, Machine(false, 1, 2))
  }

  def turnOnU11TransitionsToL01 = {
    val m = Machine(false, 1, 1)
    val s = m.simulateMachine(List(Turn))
    s.run(m) == (1, Machine(true, 0, 1))
  }

  def machineSeq = {
    val m = Machine(true, 10, 10)
    val seq = List(Coin, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Coin, Turn)
    val s = m.simulateMachine(seq)
    println("RAN : " + s.run(m))
    s.run(m) == (15, Machine(true, 6, 15))
  }
}
