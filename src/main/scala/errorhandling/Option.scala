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

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f =>
      mkMatcher(pat2) map (g =>
        f(s) && g(s)))

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _                    => None
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      v1 <- a
      v2 <- b
    } yield f(v1, v2)

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((o1: String => Boolean, o2: String => Boolean) =>
      o1(s) && o2(s))
}
