package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}


object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = map2(a, unit())((aa, _) => f(aa))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fs = parMap(as)(a => if (f(a)) List(a) else List(a))
    map(fs)(_.flatten)
  }


}