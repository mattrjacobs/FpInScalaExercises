package com.mattrjacobs.fp.state

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = this.run(s)
      f(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    State(s => fs.foldRight((Nil: List[A], s)) {
      case (f, (l, s)) => {
        val (nextValue, nextState) = f.run(s)
        (l :+ nextValue, nextState)
      }
    })
}
