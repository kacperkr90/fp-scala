package chapter8.gen

import chapter6.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG,A])

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

}

object Program {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(Gen.choose(-100, 0).sample.run(rng))
    println(Gen.choosePair(-100, 0).sample.run(rng))
    println(Gen.chooseString(10, 'a', 'z').sample.run(rng))
  }
}


