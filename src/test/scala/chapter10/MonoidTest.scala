package chapter10

import chapter10.Monoid._
import chapter6.SimpleRNG
import chapter8.gen.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class MonoidTest extends Properties("Monoid laws testing") {

  val seed: org.scalacheck.Gen[Int] = org.scalacheck.Gen.choose(Int.MinValue, Int.MaxValue)

  val rng = SimpleRNG(System.currentTimeMillis())
  val testCases = 1000

  property("string monoid") = forAll(seed)(_ => {
    !monoidLaws(stringMonoid, Gen.chooseString(5, 'A', 'z')).run(testCases, rng).isFalsified
  })

  property("list monoid") = forAll(seed)(_ => {
    !monoidLaws(listMonoid[Int], Gen.listOfN(5, Gen.choose(0, Int.MaxValue))).run(testCases, rng).isFalsified
  })

  property("intAddition monoid") = forAll(seed)(_ => {
    !monoidLaws(intAddition, Gen.choose(0, Int.MaxValue)).run(testCases, rng).isFalsified
  })

  property("intMultiplication monoid") = forAll(seed)(_ => {
    !monoidLaws(intMultiplication, Gen.choose(0, Int.MaxValue)).run(testCases, rng).isFalsified
  })

  property("booleanOr monoid") = forAll(seed)(_ => {
    !monoidLaws(booleanOr, Gen.boolean).run(testCases, rng).isFalsified
  })

  property("booleanAnd monoid") = forAll(seed)(_ => {
    !monoidLaws(booleanAnd, Gen.boolean).run(testCases, rng).isFalsified
  })

  property("optionMonoid monoid") = forAll(seed)(_ => {
    !monoidLaws(optionMonoid[Int], Gen.chooseOption(Gen.choose(0, Int.MaxValue))).run(testCases, rng).isFalsified
  })

  property("wcMonoid monoid") = forAll(seed)(_ => {
    val g1 = Gen.listOfN(2, Gen.chooseString(4, 'A', 'z'))
    val g2 = Gen.choose(0, 10)

    val gen = g1.flatMap(l =>
      g2.map(words =>
        if (words == 0) Stub(l.head)
        else Part(l.head, words, l(1))
      ))

   !monoidLaws(wcMonoid, gen).run(testCases, rng).isFalsified
  })

}
