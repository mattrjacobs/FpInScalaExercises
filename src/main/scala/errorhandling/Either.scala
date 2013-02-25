package com.mattrjacobs.fp

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(v)  => Left(v)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(v)  => Left(v)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(v)  => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      e1 <- this
      e2 <- b
    } yield f(e1, e2)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }

  def sequence[A, B](eithers: List[Either[A, B]]): Either[A, List[B]] =
    eithers match {
      case Nil     => Right(Nil)
      case x :: xs => x flatMap (r => sequence(xs).map(s => r :: s))
    }

  def traverse[A, B, C](l: List[A])(f: A => Either[B, C]): Either[B, List[C]] =
    l match {
      case Nil     => Right(Nil)
      case x :: xs => f(x) flatMap (r => traverse(xs)(f).map(s => r :: s))
    }
}
