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

  Stream.constant(1).take(5).toList should be (List(1, 1, 1, 1, 1))

  Stream.from(3).take(3).toList should be (List(3, 4, 5))

  Stream.fibs().take(8).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13))

  Stream.unfold(5)(a => if (a == 0) Option.empty else Option((a, a - 1))).toList should be (List(5, 4, 3, 2, 1))

  Stream.fibsViaUnfold().take(8).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13))

  Stream.fromViaUnfold(3).take(3).toList should be (List(3, 4, 5))

  Stream.constantViaUnfold(1).take(5).toList should be (List(1, 1, 1, 1, 1))

  stream.mapViaUnfold(_ * 2).toList should be (Stream(2, 4, 6, 8).toList)

  stream.takeViaUnfold(2).toList should be (Stream(1, 2).toList)

  stream.takeWhileViaUnfold(_ < 4).toList should be (Stream(1, 2, 3).toList)

  Stream.zipWithViaUnfold(Stream(1, 2, 3), Stream(1, 2))(_ + _).toList should be (List(2, 4))

  stream.zipAll(Stream(1, 2)).toList should be (List((Option(1), Option(1)), (Option(2), Option(2)), (Option(3), Option.empty), (Option(4), Option.empty)))

  stream.startsWith(Stream(1, 2, 2)) should be (false)
  stream.startsWith(Stream(2)) should be (false)
  stream.startsWith(Stream(1, 2, 3, 4, 5)) should be (false)
  stream.startsWith(Stream(1, 2)) should be (true)

//  stream.startsWith2(Stream(1, 2, 2)) should be (false)
//  stream.startsWith2(Stream(2)) should be (false)
//  stream.startsWith2(Stream(1, 2, 3, 4, 5)) should be (false)
//  stream.startsWith2(Stream(1, 2)) should be (true)
}
