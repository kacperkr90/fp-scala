package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  val stream: Stream[Int] = Stream(1, 2, 3, 4)

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

  stream.headOption2 should be (Option(1))

  stream.map(_ * 2).toList should be (Stream(2, 4, 6, 8).toList)

  stream.filter(_ % 2 == 0).toList should be (Stream(2, 4).toList)

  stream.append(5).toList should be (Stream(1, 2, 3, 4, 5).toList)
  stream.append(5).toList should not be Stream(1, 2, 3, 4).toList

  val stream2: Stream[Stream[Int]] = Stream(stream, stream, stream)
  stream2.flatMap(identity).toList should be (Stream(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4).toList)
}
