package com.mattrjacobs.fp.collection

import org.specs2._

class Exercise3_16 extends Specification {
  def is =
    "Exercise3-16" ^
      "List.multiAppend first empty" ! firstEmpty ^
      "List.multiAppend second empty" ! secondEmpty ^
      "List.multiAppend noneOfThree empty" ! noneEmpty

  val l1 = Cons(1, Cons(2, Cons(3, Nil)))
  val l2 = Cons(4, Cons(5, Nil))
  val l3 = Cons(6, Cons(7, Cons(8, Nil)))

  def firstEmpty = List.multiAppend(Cons(Nil, Cons(l2, Cons(l3, Nil)))) must_==
    Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil)))))
  def secondEmpty = List.multiAppend(Cons(l1, Cons(Nil, Cons(l3, Nil)))) must_==
    Cons(1, Cons(2, Cons(3, Cons(6, Cons(7, Cons(8, Nil))))))
  def noneEmpty = List.multiAppend(Cons(l1, Cons(l2, Cons(l3, Nil)))) must_==
    Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Nil))))))))
}
