package chapter8.gen

import chapter6.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def map[B](f: A => B): Gen[B] =
    flatMap(a => Gen.unit(f(a)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] =
    SGen(_ => this)

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(SimpleRNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(SimpleRNG.nonNegativeInt).map(n => n % 2 == 0))

  def listOfN_0[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State(SimpleRNG.sequence(List.fill(n)(g.sample.run))))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(listOfN(2, Gen.choose(start, stopExclusive)).sample.map(l => (l.head, l.last)))

  def toOption[A](g: Gen[A]): Gen[Option[A]] =
    Gen(g.sample.map(Option.apply))

  def chooseString(length: Int, startLetter: Char, stopLetterExclusive: Char): Gen[String] = {
    val state = listOfN(length, Gen.choose(Char.char2int(startLetter), Char.char2int(stopLetterExclusive))).sample
    Gen(state.map(ns => ns.map(_.toChar)).map(cs => cs.mkString))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val sum = g1._2 + g2._2
    val t1 = g1._2 / sum
    Gen(State(SimpleRNG.double)).flatMap(n => if (n < t1) g1._1 else g2._1)
  }

}

case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f(_).forSize(n)))

  def mapViaFlatMap[B](f: A => B): SGen[B] =
    flatMap(a => SGen(x => Gen.unit(f(a))))

  def map[B](f: A => B): SGen[B] =
    SGen(forSize.andThen(_.map(f(_))))

  def mapViaFreePointStyle[B](f: A => B): SGen[B] =
    SGen { forSize(_) map f }

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { Gen.listOfN(_, g) }

}

object Program {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(Gen.choose(-100, 0).sample.run(rng))
    println(Gen.choosePair(-100, 0).sample.run(rng))
    println(Gen.chooseString(10, 'a', 'z').sample.run(rng))
  }
}


