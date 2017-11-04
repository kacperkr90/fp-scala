package chapter3

import org.scalatest.{FlatSpec, Matchers}



class ListTest extends FlatSpec with Matchers {

  val list = List(1, 2, 3, 4, 5)

  List.tail(list) should equal (List(2, 3, 4, 5))

  List.setHead(list, 5) should equal (List(5, 2, 3, 4, 5))

  List.drop(list, 3) should equal (List(4, 5))
  List.drop(list, 10) should equal (Nil)
  List.drop(list, 0) should equal (list)

  List.dropWhile(list, (x: Int) => x < 4) should equal (List(4, 5))
  List.dropWhile(list, (x: Int) => x < 0) should equal (list)
  List.dropWhile(list, (x: Int) => x > 0) should equal (Nil)

  List.init(list) should equal (List(1, 2, 3, 4))

  List.init(List(1)) should equal (Nil)

  List.reverse(list) should equal (List(5, 4, 3, 2, 1))

  List.appendViaFoldRight(list, List(6, 7)) should equal (List(1, 2, 3, 4, 5, 6, 7))
  List.appendViaFoldLeft(list, List(6, 7)) should equal (List(1, 2, 3, 4, 5, 6, 7))

  List.flatten(List(List(1, 2, 3), List(4, 5), List(6, 7))) should equal (List(1, 2, 3, 4, 5, 6, 7))

  List.add1ToEach(list) should equal (List(2, 3, 4, 5, 6))

  List.doublesToStrings(List(1.0, 2, 3)) should equal (List("1.0", "2.0", "3.0"))

  List.filter(list)(_ > 3) should equal (List(4, 5))

  List.flatMap(List(1,2,3))(i => List(i,i)) should equal (List(1,1,2,2,3,3))
  List.flatMap2(List(1,2,3))(i => List(i,i)) should equal (List(1,1,2,2,3,3))

  List.filterViaFlatMap(list)(_ > 3) should equal (List(4, 5))

  List.zipWithSum(List(1,2,3), List(4,5,6)) should equal (List(5,7,9))
  List.zipWithSum(Nil:List[Int], Nil:List[Int]) should equal (Nil)
  List.zipWithSum(List(1,2,3), List(4,5,6,1,1)) should equal (List(5,7,9,1,1))
  List.zipWithSum(List(1,2,3,5,6), List(4,5,6)) should equal (List(5,7,9,5,6))

  List.zipWith(List(1,2,3), List(4,5,6))(_ + _) should equal (List(5,7,9))
  List.zipWith(Nil:List[Int], Nil:List[Int])(_ + _) should equal (Nil)
  List.zipWith(List(1,2,3), List(4,5,6,1,1))(_ + _) should equal (List(5,7,9,1,1))
  List.zipWith(List(1,2,3,5,6), List(4,5,6))(_ + _) should equal (List(5,7,9,5,6))

  List.hasSubsequence(List(1,2,3,4), List(1,2)) should equal (true)
  List.hasSubsequence(List(1,2,3,4), List(2,3)) should equal (true)
  List.hasSubsequence(List(1,2,3,4), List(4)) should equal (true)
}
