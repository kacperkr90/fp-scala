package chapter7

import java.util.concurrent.ForkJoinPool

import org.scalatest.{FlatSpec, Matchers}

class ParTest extends FlatSpec with Matchers {

  val es = ForkJoinPool.commonPool()

  Par.sequence(List(Par.unit(1), Par.unit(2), Par.unit(3)))(es).get should be (Par.unit(List(1, 2, 3))(es).get)

  Par.parFilter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)(es).get should be (List(2, 4, 6))

  Par.sum(IndexedSeq(1, 2, 3, 4, 5, 6))(es).get should be (21)

  Par.max(IndexedSeq(1, 2, 3, 6, 5, 4))(es).get should be (Option(6))
  Par.max(IndexedSeq(1, 2, 3, 6, 5, 4, 10))(es).get should be (Option(10))
  Par.max(IndexedSeq())(es).get should be (Option.empty)

  Par.totalWords(List())(es).get should be (0)
  Par.totalWords(List("The Grand Hotel Budapest", "Alien Requiem"))(es).get should be (6)

  Par.map3(Par.unit(1), Par.unit(2), Par.unit(3))(_ + _ + _)(es).get should be (6)
  Par.map4(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4))(_ + _ + _ + _)(es).get should be (10)
  Par.map5(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4), Par.unit(5))(_ + _ + _ + _ + _)(es).get should be (15)

  Par.choiceN(Par.unit(1))(List(Par.unit("a"), Par.unit("b"), Par.unit("c")))(es).get should be ("b")
  Par.choiceViaChoiceN(Par.unit(false))(Par.unit("a"), Par.unit("b"))(es).get should be ("b")
  Par.choiceViaChoiceN(Par.unit(true))(Par.unit("a"), Par.unit("b"))(es).get should be ("a")
}
