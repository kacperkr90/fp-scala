package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  val stream = Stream(1, 2, 3, 4)

  stream.toList should be (List(1, 2, 3, 4))

  stream.take(2).toList should be (Stream(1, 2).toList)
}
