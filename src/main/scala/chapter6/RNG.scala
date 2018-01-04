package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
      (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => (Int.MaxValue, r)
    case (i, r) if i >= 0 => (i, r)
    case (i, r) => (Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = RNG.nonNegativeInt(rng)
    val d = i.toDouble / Int.MaxValue
    (d, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, _) = RNG.double(rng)
    ((i, d), r)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = RNG.intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = RNG.double(rng)
    val (d2, r2) = RNG.double(r)
    val (d3, r3) = RNG.double(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(l: List[Int], n: Int, r: RNG): (List[Int], RNG) = {
      if (n >= 0) {
        val (i, rr) = r.nextInt
        go(i::l, n - 1, rr)
      } else
        (l, r)
    }
    go(List(), count, rng)
  }
}

object Program {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(RNG.double(rng))
    println(RNG.ints(10)(rng))
  }
}