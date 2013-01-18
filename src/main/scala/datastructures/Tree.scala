package com.mattrjacobs.fp

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)                     => 1
    case Branch(leftTree, rightTree) => 1 + size(leftTree) + size(rightTree)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value)                 => value
    case Branch(leftTree, rightTree) => maximum(leftTree) max maximum(rightTree)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(value)                 => 0
    case Branch(leftTree, rightTree) => depth(leftTree) + 1 max depth(rightTree) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value)                 => Leaf(f(value))
    case Branch(leftTree, rightTree) => Branch(map(leftTree)(f), map(rightTree)(f))
  }

  def fold[A, B](t: Tree[A])(leafFunc: A => B)(branchFunc: (B, B) => B): B =
    t match {
      case Leaf(value) => leafFunc(value)
      case Branch(leftTree, rightTree) =>
        branchFunc(fold(leftTree)(leafFunc)(branchFunc), fold(rightTree)(leafFunc)(branchFunc))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(v => 0)((l, r) => 1 + (l max r))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))
}
