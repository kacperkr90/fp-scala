package chapter4

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  val error = Left("msg")
  val right = Right(2)

  error.map((a) => 5) should be (Left("msg"))
  error.map((a) => 5) should not be Right(5)

  right.map(_ * 2) should be (Right(4))
  right.map(_ * 2) should not be Left("msg")

  error.orElse(Right(5)) should be (Right(5))
  error.orElse(Right(5)) should not be error

  right.orElse(Right(5)) should be (right)
  right.orElse(Right(5)) should not be Right(5)


}
