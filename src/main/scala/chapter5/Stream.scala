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

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Option((f(h()), t()))
      case _ => Option.empty
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n))(x => x._1 match {
      case Cons(h, t) if x._2 > 0 => Option((h(), (t(), x._2 - 1)))
      case _ => Option.empty
    })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Option(h(), t())
      case _ => Option.empty
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Option((Option(h1()), Option(h2())), (t1(), t2()))
      case (_, Cons(h2, t2)) => Option((None, Option(h2())), (Stream.empty, t2()))
      case (Cons(h1, t1), _) => Option((Option(h1()), None), (t1(), Stream.empty))
      case _ => Option.empty
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    Stream.unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Option((h1(), h2()), (t1(), t2()))
      case _ => Option.empty
    }

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll(x => x._1.exists(a => x._2.contains(a)))

  def startsWith2[B >: A](s: Stream[B]): Boolean =
    zipAll(s).forAll(x => x._1.exists(a => x._2.isEmpty || x._2.contains(a)))

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case x@Cons(_, t) => Option((x, t()))
      case _ => Option.empty
    }.append(Stream.empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.foldRight((Stream.empty:Stream[B], z))((a, bb) => {
      lazy val b = bb
      val acc = a.headOption.map(aa => f(aa, b._2)).getOrElse(b._2)
      (Stream.cons(acc, b._1), acc)
    })._1

  def scanRight2[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val acc = b
      val bb = f(a, acc._1)
      (bb, Stream.cons(bb, acc._2))
    })._2
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
    f(z).map(x => Stream.cons(x._1, unfold(x._2)(f))).getOrElse(Stream.empty: Stream[A])

  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1))(x => Option(x._1, (x._2, x._1 + x._2)))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Option(x, x + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Option(a, a))

  def onesViaUnfold(): Stream[Int] =
    unfold(1)(_ => Option(1, 1))

  def zipWithViaUnfold[A, B](l1: Stream[A], l2: Stream[A])(f: (A, A) => B): Stream[B] =
    unfold((l1, l2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Option(f(h1(), h2()), (t1(), t2()))
      case _ => Option.empty
    }

}