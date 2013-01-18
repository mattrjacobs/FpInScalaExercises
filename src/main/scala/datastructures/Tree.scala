package com.mattrjacobs.fp

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)                     => 1
    case Branch(leftTree, rightTree) => Tree.size(leftTree) + Tree.size(rightTree)
  }
}
