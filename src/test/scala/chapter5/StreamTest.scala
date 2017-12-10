package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  val stream = Stream(1, 2, 3, 4)

  stream.toList should be (List(1, 2, 3, 4))

  stream.take(2).toList should be (Stream(1, 2).toList)
  stream.take(1).toList should be (Stream(1).toList)
  Stream(1).take(1).toList should be (Stream(1).toList)
  stream.take(5).toList should be (stream.toList)
  stream.take(-1).toList should be (List())

  stream.drop(2).toList should be (Stream(3, 4).toList)
  stream.drop(5).toList should be (List())
  stream.drop(-1).toList should be (List())

  stream.takeWhile(_ < 4).toList should be (Stream(1, 2, 3).toList)

  stream.forAll(_ < 4) should be (false)

  stream.takeWhile2(_ < 4).toList should be (Stream(1, 2, 3).toList)

}
