package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def apply[A](a: A): Option[A] =
    if (a != null) Some(a)
    else None

  def of[A](xs: Seq[A]): Option[Seq[A]] = {
    if (xs == Nil)
      None
    else
      Some(xs)
  }

  def mean(xs: Seq[Double]): Option[Double] =
    of(xs)
        .map(as => as.sum / as.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
        .map(m => xs.map(x => math.pow(x - m, 2)))
        .flatMap(mean)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Option(Nil:List[A]))(map2(_, _)(_::_))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Option(Nil:List[B]))((x, acc) => map2(f(x), acc)(_::_))

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}