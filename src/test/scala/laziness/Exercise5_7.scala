package com.mattrjacobs.fp.laziness

import org.specs2._

class Exercise5_7 extends Specification {
  import Stream._

  val s = cons(1, cons(2, cons(3, Stream.empty[Int])))
  val f = (x: Int) => x + 8
  val g = (x: Int) => {
    val str = (x * 2).toString
    cons(str, cons(str, Stream.empty[String]))
  }

  def is =
    "Exercise 5-7" ^
      "Stream.map" ^
      "empty" ! mapEmpty ^
      "non-empty" ! mapNonEmpty ^
      p ^
      "Stream.filter" ^
      "all true" ! filterAllTrue ^
      "all false" ! filterAllFalse ^
      "some true" ! filterSomeTrue ^
      p ^
      "Stream.append" ^
      "empty.append(empty)" ! appendEmptyEmpty ^
      "empty.append(nonEmpty)" ! appendEmptyNonEmpty ^
      "nonEmpty.append(empty)" ! appendNonEmptyEmpty ^
      "nonEmpty.append(nonEmpty)" ! appendNonEmptyNonEmpty ^
      p ^
      "Stream.flatMap" ^
      "empty flatMapped is empty" ! emptyFlatMap ^
      "flatMap with identity" ! flatMapIdentity ^
      "flatMap with doubled string" ! flatMapDoubledString

  def mapEmpty = Stream.empty[Int].map(f).toList must_== Nil

  def mapNonEmpty = s.map(f).toList must_== List(9, 10, 11)

  def filterAllTrue = s.filter(_ => true).toList must_== List(1, 2, 3)

  def filterAllFalse = s.filter(_ => false).toList must_== Nil

  def filterSomeTrue = s.filter(n => n % 2 == 1).toList must_== List(1, 3)

  def appendEmptyEmpty = Stream.empty[Int].append(Stream.empty[Int]).toList must_== Nil

  def appendEmptyNonEmpty = Stream.empty[Int].append(s).toList must_== List(1, 2, 3)

  def appendNonEmptyEmpty = s.append(Stream.empty[Int]).toList must_== List(1, 2, 3)

  def appendNonEmptyNonEmpty = s.append(s).toList must_== List(1, 2, 3, 1, 2, 3)

  def emptyFlatMap = Stream.empty[Int].flatMap(g).toList must_== Nil

  def flatMapIdentity = s.flatMap(x => cons(x, Stream.empty[Int])).toList must_== List(1, 2, 3)

  def flatMapDoubledString = s.flatMap(g).toList must_== List("2", "2", "4", "4", "6", "6")
}
