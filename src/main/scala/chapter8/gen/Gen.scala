package chapter8.gen

import chapter6.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG,A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(SimpleRNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

//  def boolean: Gen[Boolean] =


}

object Program {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(Gen.choose(-100, 0).sample.run(rng))
  }
}


