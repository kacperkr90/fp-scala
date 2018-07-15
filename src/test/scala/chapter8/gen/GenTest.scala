package chapter8.gen

import chapter6.SimpleRNG
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class GenTest extends Properties("Testing the testing") {

  val seeds: org.scalacheck.Gen[Int] = org.scalacheck.Gen.choose(Int.MinValue, Int.MaxValue)
  property("should properly generate random negative number in state") = forAll(seeds)(seed => {
    val randomInt = Gen.choose(-100, 0).sample.run(SimpleRNG(seed))._1
    randomInt >= -100 && randomInt < 0
  })

  property("should properly generate random positive number in state") = forAll(seeds)(seed => {
    val randomInt = Gen.choose(1, 100).sample.run(SimpleRNG(seed))._1
    randomInt >= 1 && randomInt < 100
  })

  property("should properly generate zero as random in state") = forAll(seeds)(seed => {
    val randomInt = Gen.choose(0, 1).sample.run(SimpleRNG(seed))._1
    randomInt >= 0 && randomInt < 1
  })

  property("should properly generate constant number in state") = forAll(seeds)(seed => {
    Gen.unit(seed).sample.run(SimpleRNG(seed))._1 == seed
  })

  property("should generate list of random values based on generator") = forAll(seeds)(seed => {
    val ints = Gen.listOfN_0(5, Gen.choose(-100, 100)).sample.run(SimpleRNG(seed))._1
    ints.size == 5 && ints.forall(n => n >= -100 && n < 100)
  })

  property("should generate pairs of integer") = forAll(seeds)(seed => {
    val pair = Gen.choosePair(-10, 10).sample.run(SimpleRNG(seed))._1
    val first = pair._1
    val second = pair._2
    (first >= -10 && first < 10) && (second >= -10 && second < 10)
  })

  property("should properly generate Gen[Option[A] from Gen[A]") = forAll(seeds)(seed => {
    val g = Gen.choose(10, -10)
    Option(g.sample.run(SimpleRNG(1))._1) == Gen.toOption(g).sample.run(SimpleRNG(1))._1
  })

}
