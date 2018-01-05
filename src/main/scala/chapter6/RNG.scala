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

  type Rand[+A] = RNG => (A, RNG)

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
      if (n > 0) {
        val (i, rr) = r.nextInt
        go(i::l, n - 1, rr)
      } else
        (l, r)
    }
    go(List(), count, rng)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {

  }


  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // (rng -> (a, rng)) -> (rng -> (b, rng)) -> ((a, b) -> c) -> (rng -> (c, rng)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   rng => {
     val (a, r) = ra(rng)
     val (b, _) = rb(rng)
     (f(a, b), r)
   }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((List():List[A], rng))((s, res) => {
        val (as, prevR) = res
        val (a, nextR) = s(prevR)
        (a::as, nextR)
      })
    }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((s, res) => map2(s, res)(_ :: _))

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(_.nextInt))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(map(nonNegativeInt)(_ % n))(i => )
}

object Program {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(RNG.double(rng))
    println(RNG.ints(3)(rng))
    println(RNG.ints2(3)(rng))
  }
}