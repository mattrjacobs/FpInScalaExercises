package com.mattrjacobs.fp.testing

trait Gen[A] {

}

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check = this.check && p.check
  }
}

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] =
    new Gen[List[A]] {}
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    new Gen[List[A]] {}
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop =
    new Prop {
      def check = true
    }
}
