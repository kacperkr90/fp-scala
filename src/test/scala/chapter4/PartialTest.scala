package chapter4

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class PartialTest extends FlatSpec with Matchers  {

  val error: Partial[Int, Int] = Errors(Seq(2))
  val right: Partial[Int, Int] = Success(2)

  error.map((a) => 5) should be (error)
  error.map((a) => 5) should not be Success(5)

  right.map(_ * 2) should be (Success(4))
  right.map(_ * 2) should not be error

  error.orElse(Success(5)) should be (Success(5))
  error.orElse(Success(5)) should not be error

  right.orElse(Success(5)) should be (right)
  right.orElse(Success(5)) should not be Success(5)

  right.map2(right)(_ * _) should be (Success(4))
  right.map2(error)(_ * _) should be (error)
  error.map2(right)(_ * _) should be (error)

  val list = List(Success(1), Success(2), Success(3))
  val list2 = List(Success(1), error, Success(2), Success(3), Errors(Seq(4)))
  Partial.sequence(list) should be (Success(List(1, 2, 3)))
  Partial.sequence(list2) should be (Errors(Seq(2, 4)))

  Partial.traverse(list)(identity) should be (Success(List(1, 2, 3)))
  Partial.traverse(list2)(identity) should be (Errors(Seq(2, 4)))
  
}
