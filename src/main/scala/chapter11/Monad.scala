package chapter11

import chapter4.Option
import chapter5.Stream
import chapter6.State
import chapter7.Par.Par
import chapter7.Par
import chapter8.gen.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(Nil:List[A]))((a, b) => map2(a, b)(_ :: _))

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la map f)
}


object Monad {

  val genMonad: Monad[Gen] = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: A): Par[A] = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: A): Option[A] = Option(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: A): Stream[A] = Stream(a)
    override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa flatMap f
  }

  val listMonad: Monad[chapter3.List] = new Monad[chapter3.List] {
    override def unit[A](a: A): chapter3.List[A] = chapter3.List(a)
    override def flatMap[A, B](fa: chapter3.List[A])(f: A => chapter3.List[B]): chapter3.List[B] = chapter3.List.flatMap(fa)(f)
  }

  val stateMonad: Monad[State] = ???

}
