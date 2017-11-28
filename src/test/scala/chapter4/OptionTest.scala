package chapter4

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers  {

  val option = Some(5)

  option.map(_*2) should equal (Some(10))
  (None:Option[Int]).map(_*2) should equal (None)

  option.flatMap(Some(_)) should equal (option)
  (None:Option[Int]).map(Some(_)) should equal (None)

  option.getOrElse(2) should equal(5)
  (None:Option[Int]).getOrElse(2) should equal (2)

  option.orElse(Some(2)) should be (Some(5))
  (None:Option[Int]).orElse(Some(2)) should be (Some(2))

  option.filter(_>2) should be (option)
  option.filter(_==2) should be (None)
  (None:Option[Int]).filter(_>2) should be (None)

  val seq = Seq(1.0, 2, 3, 4, 5)
  Option.variance(seq) should be (Some(2.0))
  Option.variance(Nil) should be (None)
  Option.variance(Seq()) should be (None)

  val list = List(Some(1), Some(2), Some(3))
  val list2 = List(Some(1), None, Some(3))
  Option.sequence(list) should be (Some(List(1, 2, 3)))
  Option.sequence(list2) should be (None)
  Option.sequence2(list) should be (Some(List(1, 2, 3)))
  Option.sequence2(list2) should be (None)
}
