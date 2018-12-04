package chapter11

import chapter4.Option
import chapter5.Stream
import chapter6.{RNG,SimpleRNG, State}
import chapter7.Par
import chapter7.Par.Par
import chapter8.gen.Gen

trait Monad[F[_]] extends Functor[F] {self =>
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

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // first try, ugly (one iteration through list)
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(Nil:List[A]))((m, acc) => map2(f(m), acc)((b, as) => if (b) m::as else as))

  // two iterations but cleaner (i think)
  def filterM2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    traverse(ms)(m => product(unit(m), f(m))).map(_.filter(_._2).map(_._1))
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose(identity[F[A]], f)(fa)

  // EXERCISE 11.10
  // compose(unit, f)(v) == f(v)
  // (a => flatMap(f(a))(unit))(v) == f(v)
  // flatMap(f(v))(unit) == f(v)
  // flatMap(x)(unit) == x
  // x == x

  // compose(f, unit)(v) == f(v)
  // (a => flatMap(unit(a))(f))(v) == f(v)
  // flatMap(unit(v))(f) == f(v) | after computing unit(v) and then passing it to flatMap we get f(v)
  // f(v) == f(v)

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(x => x)

  def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

  def _flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

  // helper class for cleaner code
  implicit def operators[A](m: F[A]): MonadOps[A] = MonadOps[A](m)

  case class MonadOps[A](m: F[A]) {
    def map[B](f: A => B): F[B] = self.map(m)(f)
    def flatMap[B](f: A => F[B]): F[B] = self.flatMap(m)(f)
  }
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

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] =
      Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] =
      f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: A): Id[A] = Id(a)
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

  type IntState[A] = State[Int, A]

  val intStateMonad: Monad[IntState] = new Monad[IntState] {
    override def unit[A](a: A): IntState[A] = State(s => (a, s))
    override def flatMap[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = fa.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
  }

  def main(args: Array[String]): Unit = {
    val ints = List(1, 2, 3, 5, 6, 7)
    val res = optionMonad.filterM(ints)(n => optionMonad.unit(n % 2 == 0))
    println(res)

    println(optionMonad.flatMapViaCompose(Option(4))(x => Option(x * 2)))
    println(optionMonad.join(Option(Option(4))))

    println(Id("Hello, ") flatMap (a => Id("monad!") flatMap(b => Id(a + b))))
    println(for {
      a <- Id("Hello, ")
      b <- Id("moand!")
    } yield a + b)

    val rng = SimpleRNG(1)
    val state = State(SimpleRNG.nonNegativeInt)
    val state2 = State(SimpleRNG.nonNegativeEven)
    println(stateMonad[RNG].replicateM[Int](4, state).run(rng))
    println(stateMonad[RNG].map2(state, state2)(_ + _).run(rng))
  }

}
