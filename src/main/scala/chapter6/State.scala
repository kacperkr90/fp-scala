package chapter6

//unit , map , map2 , flatMap , and sequence
case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] =
    State(this.run.andThen(x => (f(x._1), x._2)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(this.run.andThen(xa => sb.run.andThen(xb => (f(xa._1, xb._1), xb._2))(xa._2)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(this.run.andThen(x => f(x._1).run(x._2)))


}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(State.unit[S, List[A]](List()))(_.map2(_)(_::_))
}