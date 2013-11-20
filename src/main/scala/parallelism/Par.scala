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

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }

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
