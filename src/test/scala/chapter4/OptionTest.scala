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
}
