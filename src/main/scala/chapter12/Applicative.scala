package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List():List[B]))((a, bs) => map2(f(a), bs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fa, fab)((a, f) => f(a))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // my version, long :(
  def map2ViaApply_[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val g: F[B => (A, B)] = mapViaApply(fa)(a => (b: B) => (a, b))
    val fab: F[(A, B)] = apply(g)(fb)
    apply(unit(f.tupled))(fab)
  }

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(mapViaApply(fa)(f.curried))(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(mapViaApply(fa)(f.curried))(fb))(fc)

  def map3_[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val g = (a: A, b: B) => (c: C) => f(a, b, c)
    apply(map2ViaApply(fa, fb)(g))(fc)
  }

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(mapViaApply(fa)(f.curried))(fb))(fc))(fd)

  def map4_[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val g = (a: A, b: B, c: C) => (d: D) => f(a, b, c, d)
    apply(map3(fa, fb, fc)(g))(fd)
  }
}


trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
}
