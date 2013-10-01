package com.mattrjacobs.fp.laziness

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case Some((element, rest)) => element :: rest.toList
    case None                  => Nil
  }

  def take(n: Int): Stream[A] = if (n == 0) {
    Stream.empty
  } else {
    uncons match {
      case Some((element, rest)) => new Stream[A] {
        lazy val uncons = Some((element, rest.take(n - 1)))
      }
      case None => Stream.empty
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((element, rest)) if p(element) => new Stream[A] {
      lazy val uncons = Some((element, rest.takeWhile(p)))
    }
    case Some((element, rest)) if !p(element) => rest.takeWhile(p)
    case None                                 => Stream.empty
  }

  def forAll(p: A => Boolean): Boolean = uncons match {
    case Some((element, rest)) if !p(element) => false
    case Some((element, rest)) if p(element)  => rest.forAll(p)
    case None                                 => true
  }
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    def uncons = scala.None
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
    lazy val uncons = Some((hd, tl))
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
