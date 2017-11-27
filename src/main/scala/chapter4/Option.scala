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

}