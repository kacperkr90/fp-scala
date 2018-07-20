package chapter8

import chapter6.SimpleRNG
import chapter8.gen.Gen

object Ch8 {

  def sum: List[Int] => Int =
    list => list.sum

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(Prop.forAll(Gen.choose(0, 101))(_ % 2 == 0).run(100, rng))
  }
}
