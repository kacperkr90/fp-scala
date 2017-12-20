package chapter5

import chapter2.FibonacciModule

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(_, t) if n == 0 => this
    case _ => Stream.empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def foldLeftViaFoldRight[B](z: => B)(f: (=> B, A) => B): B =
    foldRight((b: B) => b)((a, g) => (b) => g(f(b, a)))(z)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty:Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  def headOption2: Option[A] =
    foldLeftViaFoldRight(Option.empty:Option[A])((b, a) => b.orElse(Option(a)))

  def headOption3: Option[A] =
    foldRight(Option.empty:Option[A])((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty:Stream[B])((h, t) => Stream.cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty:Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](x: => B): Stream[B] =
    foldRight(Stream.cons(x, Stream.empty))((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty:Stream[B])(f(_).foldRight(_)(Stream.cons(_, _)))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] =
    Stream.from(0).map(FibonacciModule.fibonacciTailRec)

  def fibs2(): Stream[Int] = {
    def go(a: Int, b: Int):Stream[Int] =
      Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(x => Stream.cons(x._1, unfold(x._2)(f))).getOrElse(Stream.empty:Stream[A])

}