package com.mattrjacobs.fp.parallelism

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  def sum(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a, b) => a + b)

  def sumDivideAndConquer(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sumDivideAndConquer(l), sumDivideAndConquer(r))(_ + _)
    }

  def unit[A](a: A): Par[A] = es =>
    UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true
    override def get(timeout: Long, units: TimeUnit) = get
    override def isCancelled = false
    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => async(f(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def map2[A, B, C](first: Par[A], second: Par[B])(f: (A, B) => C): Par[C] = es => {
    val firstFuture = first(es)
    val secondFuture = second(es)
    UnitFuture(f(firstFuture.get, secondFuture.get))
  }

  def map3[A, B, C, D](first: Par[A], second: Par[B], third: Par[C])(f: (A, B, C) => D): Par[D] = {
    val abApplied: Par[C => D] =
      map2(first, second)(f.curried(_)(_))
    map2(abApplied, third)(_ apply _)
  }

  def map4[A, B, C, D, E](first: Par[A], second: Par[B], third: Par[C], fourth: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val abApplied: Par[C => D => E] =
      map2(first, second)(f.curried(_)(_))
    val cApplied: Par[D => E] =
      map2(abApplied, third)(_ apply _)
    map2(cApplied, fourth)(_ apply _)
  }

  def map5[A, B, C, D, E, F](first: Par[A], second: Par[B], third: Par[C], fourth: Par[D], fifth: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    val abApplied: Par[C => D => E => F] =
      map2(first, second)(f.curried(_)(_))
    val cApplied: Par[D => E => F] =
      map2(abApplied, third)(_ apply _)
    val dApplied: Par[E => F] =
      map2(cApplied, fourth)(_ apply _)
    map2(dApplied, fifth)(_ apply _)
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val nested: List[Par[List[A]]] = l.map(asyncF { (a: A) =>
      f(a) match {
        case true  => List(a)
        case false => Nil
      }
    })

    map(sequence(nested))(_.flatten)
  }

  def parCount(l: List[String]): Par[Int] = {
    val ints: List[Par[Int]] = l.map(asyncF { wordCount })
    parReduce(sequence(ints))(_ + _)(0)
  }

  def parReduce[A](l: Par[List[A]])(f: (A, A) => A)(z: A): Par[A] = {
    val x: List[A] => A = (s: List[A]) => s match {
      case Nil          => z
      case nonEmptyList => nonEmptyList.reduceLeft(f)
    }
    map(l)(x)
  }

  def wordCount(s: String) = s.split(" ").size

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(Nil): Par[List[A]]) {
      case (parListA, parA) =>
        map2(parListA, parA)((hd, rest) => hd :: rest)
    }

  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      override def call = a(es).get
    })
}
