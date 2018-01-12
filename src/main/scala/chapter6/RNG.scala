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

object SimpleRNG {

  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => (Int.MaxValue, r)
    case (i, r) if i >= 0 => (i, r)
    case (i, r) => (Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val d = i.toDouble / Int.MaxValue
    (d, r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, _) = double(rng)
    ((i, d), r)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
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

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    s.andThen(x => (f(x._1), x._2))

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
    sequence(List.fill(count)((x:RNG) => x.nextInt))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    f.andThen(x => g(x._1)(x._2))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def map2ViaFlatMap2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

object Program {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(100)
    println(SimpleRNG.double(rng))
    println(SimpleRNG.ints(3)(rng))
    println(SimpleRNG.ints2(3)(rng))
  }
}