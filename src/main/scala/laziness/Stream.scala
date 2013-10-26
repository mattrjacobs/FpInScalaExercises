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

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this)((s: Stream[A]) => s.uncons.flatMap {
      case ((head, tail)) if (p(head)) => Some((head, tail))
      case _                           => None
    })

  def takeWhile(p: A => Boolean): Stream[A] =
    takeWhileViaUnfold(p)

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

  def zip[B](b: Stream[B]): Stream[(A, B)] = Stream.unfold((this, b)) {
    case (a, b) => (a.uncons, b.uncons) match {
      case (Some((headA, tailA)), (Some((headB, tailB)))) =>
        Some(((headA, headB), (tailA, tailB)))
      case _ => None
    }
  }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((Some(this): Option[Stream[A]], Some(b): Option[Stream[B]])) {
      case (a, b) => (a.flatMap(_.uncons), b.flatMap(_.uncons)) match {
        case (Some((headA, tailA)), (Some((headB, tailB)))) =>
          Some(((Some(headA), Some(headB)), (Some(tailA), Some(tailB))))
        case (Some((headA, tailA)), None) =>
          Some(((Some(headA), None), (Some(tailA), None)))
        case (None, Some((headB, tailB))) =>
          Some((None, Some(headB)), (None, Some(tailB)))
        case (None, None) =>
          None
      }
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold((this, true)) {
      case (s, keepGoing) => (s.uncons, keepGoing) match {
        case (_, false)              => None
        case (Some((head, tail)), _) => Some(s, (tail, true))
        case (None, _)               => Some(empty, (empty, false))
      }
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(t => t.foldRight(z)(f))
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

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.zipAll(s2).forAll {
      case (Some(a), Some(b)) if a == b => true
      case (Some(a), Some(b))           => false
      case (Some(_), None)              => true
      case (None, Some(_))              => false
      case (None, None)                 => true
    }

  def hasSubsequence[A](s: Stream[A], s2: Stream[A]): Boolean =
    s.tails exists (startsWith(_, s2))
}
