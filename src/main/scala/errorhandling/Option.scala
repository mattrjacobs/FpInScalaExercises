package com.mattrjacobs.fp

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None    => None
  }

  def flatMap_patternMatch[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None    => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None    => default
  }

  def orElse_patternMatch[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => Some(v)
    case None    => ob
  }

  def orElse[B >: A](ob: Option[B]): Option[B] =
    this.map(Some(_)) getOrElse ob

  def filter_patternMatch(f: A => Boolean): Option[A] = this match {
    case Some(v) if f(v) => Some(v)
    case Some(v)         => None
    case None            => None
  }

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(v => if (f(v)) Some(v) else None)
}

case class Some[+A](v: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = xs.size match {
    case 0 => None
    case _ => Some(xs.sum / xs.size)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    def varianceGivenMean(m: Double, xs: Seq[Double]): Option[Double] =
      mean(xs.map(x => math.pow(x - m, 2)))

    mean(xs).flatMap(varianceGivenMean(_, xs))
  }
}
