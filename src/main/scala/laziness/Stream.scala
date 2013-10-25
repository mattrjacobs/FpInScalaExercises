package com.mattrjacobs.fp.laziness

trait Stream[+A] {
  import Stream._

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case Some((element, rest)) => element :: rest.toList
    case None                  => Nil
  }

  def take(n: Int): Stream[A] = takeViaUnfold(n)

  def takeViaUncons(n: Int): Stream[A] = if (n == 0) {
    empty
  } else {
    uncons match {
      case Some((element, rest)) => new Stream[A] {
        lazy val uncons = Some((element, rest.take(n - 1)))
      }
      case None => empty
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (s, remaining) if remaining > 0 => s.uncons.map {
      case ((head, tail)) => (head, (tail, remaining - 1))
    }
    case _ => None
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((element, rest)) => f(element, rest.foldRight(z)(f))
      case None                  => z
    }

  def takeWhileViaPatternMatch(p: A => Boolean): Stream[A] = uncons match {
    case Some((element, rest)) if p(element) => new Stream[A] {
      lazy val uncons = Some((element, rest.takeWhile(p)))
    }
    case Some((element, rest)) if !p(element) => rest.takeWhile(p)
    case None                                 => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => p(h) match {
      case true  => cons(h, t)
      case false => empty
    })

  def takeWhile(p: A => Boolean): Stream[A] =
    takeWhileViaFoldRight(p)

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((h, t) => p(h) || t)

  def forAll(p: A => Boolean): Boolean = uncons match {
    case Some((element, rest)) if !p(element) => false
    case Some((element, rest)) if p(element)  => rest.forAll(p)
    case None                                 => true
  }

  def map[B](f: A => B): Stream[B] = mapViaUnfold(f)

  def mapViaFold[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this)((s: Stream[A]) => s.uncons.map {
      case ((head, tail)) => (f(head), tail)
    })

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => p(h) match {
      case true  => cons(h, t)
      case false => t
    })

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).uncons.map(_._1)
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

  def constant[A](a: A): Stream[A] = constantViaUnfold(a)
  def constantCons[A](a: A): Stream[A] = cons(a, constant(a))
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val ones: Stream[Int] = onesViaUnfold
  val onesCons: Stream[Int] = constantCons(1)
  val onesViaUnfold: Stream[Int] = constantViaUnfold(1)

  def from(i: Int): Stream[Int] = fromViaUnfold(i)
  def fromCons(i: Int): Stream[Int] = cons(i, from(i + 1))
  def fromViaUnfold(i: Int): Stream[Int] =
    unfold(i)(n => Some(n, n + 1))

  def fibs(): Stream[Int] = fibsViaUnfold()
  def fibsCons(): Stream[Int] = fibHelper(0, 1)
  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1)) {
      case (a, b) => Some((a, (b, a + b)))
    }

  def fibHelper(a: Int, b: Int): Stream[Int] =
    cons(a, cons(b, fibHelper(a + b, a + (2 * b))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None         => empty
    }
}
