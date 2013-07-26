package com.mattrjacobs.fp

trait Stream[+A] {
  def uncons: scala.Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case scala.Some((element, rest)) => element :: rest.toList
    case scala.None                  => Nil
  }
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = scala.None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = scala.Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
