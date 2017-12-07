package chapter4

trait Partial[+E, +A] {
  def map[B](f: A => B): Partial[E, B] = this match {
    case Success(a) => Success(f(a))
    case Errors(a) => Errors(a)
  }

  def flatMap[EE >: E, B](f: A => Partial[EE, B]): Partial[EE, B] = this match {
    case Success(a) => f(a)
    case Errors(a) => Errors(a)
  }

  def orElse[EE >: E,B >: A](b: => Partial[EE, B]): Partial[EE, B] = this match {
    case Errors(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Partial[EE, B])(f: (A, B) => C): Partial[EE, C] = (this, b) match {
    case (Errors(aa), Errors(bb)) => Errors(aa ++ bb)
    case _ => this.flatMap(aa => b.map(bb => f(aa, bb)))
  }

}
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

object Partial {
  
  def sequence[E, A](es: List[Partial[E, A]]): Partial[E, List[A]] =
    es.foldRight(Success(Nil:List[A]):Partial[E, List[A]])(_.map2(_)(_ :: _))


  def traverse[E, A, B](as: List[A])(f: A => Partial[E, B]): Partial[E, List[B]] =
    as.foldRight(Success(Nil:List[B]):Partial[E, List[B]])(f(_).map2(_)(_ :: _))

}