package chapter7

import java.util.concurrent.Executors

import org.scalatest.{FlatSpec, Matchers}

class ParTest extends FlatSpec with Matchers {

  val es = Executors.newSingleThreadExecutor()

  Par.sequence(List(Par.unit(1), Par.unit(2), Par.unit(3)))(es).get should be (Par.unit(List(1, 2, 3))(es).get)

  Par.parFilter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)(es).get should be (List(2, 4, 6))
}
