package chapter4

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  val error: Either[Int, Int] = Left(2)
  val right: Either[Int, Int] = Right(2)

  error.map((a) => 5) should be (error)
  error.map((a) => 5) should not be Right(5)

  right.map(_ * 2) should be (Right(4))
  right.map(_ * 2) should not be error

  error.orElse(Right(5)) should be (Right(5))
  error.orElse(Right(5)) should not be error

  right.orElse(Right(5)) should be (right)
  right.orElse(Right(5)) should not be Right(5)

  right.map2(right)(_ * _) should be (Right(4))
  right.map2(error)(_ * _) should be (error)
  error.map2(right)(_ * _) should be (error)

  val list = List(Right(1), Right(2), Right(3))
  val list2 = List(Right(1), error, Right(2), Right(3), Left(4))
  Either.sequence(list) should be (Right(List(1, 2, 3)))
  Either.sequence(list2) should be (error)

  Either.traverse(list)(identity) should be (Right(List(1, 2, 3)))
  Either.traverse(list2)(identity) should be (error)
}
