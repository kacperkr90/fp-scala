package chapter8

import chapter8.Ch8.sum
import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop.forAll

class Ch8Test extends Properties("Practicing properties") {

  val list: Gen[List[Int]] = Gen.listOf(Gen.choose(0, 20))
  property("list sum should equals to reversed list sum") = forAll(list)(ns => sum(ns) == sum(ns.reverse))

  val fives: Gen[List[Int]] = Gen.listOf(Gen.const(5))
  property("same value list sum should equals to value multiplied by list lenght") = forAll(fives)(ns => sum(ns) == ns.size * 5)

  val list1: Gen[List[Int]] = Gen.nonEmptyListOf(Gen.const(5))
  property("should find max number in non-empty list") = forAll(list1)(ns => ns.max == 5)

}
