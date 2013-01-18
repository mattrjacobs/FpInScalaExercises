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
}
