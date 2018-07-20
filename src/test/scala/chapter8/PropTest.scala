package chapter8

import chapter6.SimpleRNG
import chapter8.gen.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class PropTest extends Properties("Prop")  {

  val seed: org.scalacheck.Gen[Int] = org.scalacheck.Gen.choose(Int.MinValue, Int.MaxValue)
  val p1: Prop = Prop.forAll(Gen.choose(0, 100))(_ < 100)
  val p2: Prop = Prop.forAll(Gen.choose(-100, 0))(_ < 0)
  val p3: Prop = Prop.forAll(Gen.choose(-100, 0))(_ > 0)
  val p4: Prop = Prop.forAll(Gen.choose(0, 100))(_ > 100)

  property("should properly combine props with && function") = forAll(seed)(s => {
    !p1.&&(p2).run(100, SimpleRNG(s)).isFalsified &&
      p3.&&(p1).run(100, SimpleRNG(s)).isFalsified &&
      p1.&&(p3).run(100, SimpleRNG(s)).isFalsified &&
      p3.&&(p4).run(100, SimpleRNG(s)).isFalsified
  })

  property("should properly combine props with || function") = forAll(seed)(s => {
    !p1.||(p2).run(100, SimpleRNG(s)).isFalsified &&
      !p3.||(p1).run(100, SimpleRNG(s)).isFalsified &&
      !p1.||(p3).run(100, SimpleRNG(s)).isFalsified &&
      p3.||(p4).run(100, SimpleRNG(s)).isFalsified
  })

}
