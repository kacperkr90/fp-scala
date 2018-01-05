package chapter6

import org.scalatest.{FlatSpec, Matchers}

class RNGTest extends FlatSpec with Matchers {

  val rng = SimpleRNG(100)

  RNG.ints(3)(rng) should be eq (RNG.ints2(4)(rng))
}
