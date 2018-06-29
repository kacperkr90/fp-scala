package chapter7

import java.util.concurrent._

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
  def mapViaFlatMap[A, B](a: Par[A])(f: A => B): Par[B] = flatMap(a)(aa => lazyUnit(f(aa)))

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => f(a(es).get)(es)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fs = parMap(as)(a => if (f(a)) List(a) else List())
    map(fs)(_.flatten)
  }

  def foldAsync[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B)(g: (B, B) => B): Par[B] =
    if (as.length <= 1)
      unit(as.headOption.map(a => f(a, z)) getOrElse z)
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(fork(foldAsync(l)(z)(f)(g)), fork(foldAsync(r)(z)(f)(g)))(g(_,_))
    }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    foldAsync(ints)(0)(_ + _)(_ + _)

  def max(ints: IndexedSeq[Int]): Par[Option[Int]] = {
    def optionMax = (a: Int, b: Option[Int]) =>
      b.map(bb => Option(Math.max(a, bb)))
        .getOrElse(Option(a))

    foldAsync(ints)(Option.empty:Option[Int])(optionMax)((z1, z2) => z1.flatMap(z => optionMax(z, z2)))
  }

  def totalWords(ps: List[String]): Par[Int] = {
    val s = ps.map(asyncF(_.split(" "))).map(map(_)(_.length))
    flatMap(sequence(s))(ws => sum(ws.toIndexedSeq))
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
    map2(a, map2(b, c)((bb, cc) => (aa: A) => f(aa, bb, cc)))((aa, g) => g(aa))

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
    map2(a, map3(b, c, d)((bb, cc, dd) => (aa: A) => f(aa, bb, cc, dd)))((aa, g) => g(aa))

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] =
    map2(a, map4(b, c, d, e)((bb, cc, dd, ee) => (aa: A) => f(aa, bb, cc, dd, ee)))((aa, g) => g(aa))

  // EXERCISE 7.7
  // map(map(y)(g))(f) == map(y)(f compose g)             || assuming map(y)(id) == y and g == id
  // map(y)(f) == map(y)(f compose id)                    || assuming f: a -> b and g: a -> a, so:
  //                                                      || f compose g is (a -> a) -> b which can be simplified to a -> b, which is f
  // map(y)(f) == map(y)(f)

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val value = run(es)(n).get
      choices(value)(es)
    }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => {
      val n = map(cond)(flag => if (flag) 0 else 1)
      choiceN(n)(List(t, f))(es)
    }

  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val value = run(es)(key).get
      choices(value)(es)
    }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(pa(es).get)(es)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => chooser(cond)(if (_) t else f)(es)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => chooser(n)(choices(_))(es)

}