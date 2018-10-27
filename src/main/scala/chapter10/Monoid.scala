package chapter10

import java.util.concurrent.Executors

import chapter8.Prop
import chapter8.gen.Gen
import chapter7.Par._


trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = List()
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
    override def zero: A => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    import m._

    def zeroLaw:Prop = Prop.forAll(gen)(a => op(a, zero) == a)
    def opLaw: Prop = Prop.forAll(Gen.listOfN(3, gen)){ case x::y::z::Nil => op(op(x, y), z) == op(x, op(y, z))}

    zeroLaw && opLaw
  }

  def concatenated[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenated(as.map(f), m)

  def foldLeftViaFoldMap[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  def foldRightViaFoldMap[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(a, b))(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.splitAt(v.size / 2) match {
    case (Seq(a), Seq()) => f(a)
    case (Seq(), Seq(a)) => f(a)
    case (Seq(a1), Seq(b1)) => m.op(f(a1), f(b1))
    case (v1, v2) => m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1, a2)(m.op)
    override def zero: Par[A] = lazyUnit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B]  =
    flatMap(parMap(v.toList)(asyncF(f))) {
      list => foldMapV(list.toIndexedSeq, par(m))(identity)
    }

  def orderedMonoid: Monoid[Option[(Int, Int, Boolean)]] = new Monoid[Option[(Int, Int, Boolean)]] {
    override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
      (a1, a2) match {
        case (Some((x1, y1, f1)), Some((x2, y2, f2))) => Some((x1 min x2, y1 max y2, y1 <= x2 && f1 && f2))
        case (x, None) => x
        case (None, x) => x
      }

    override def zero: Option[(Int, Int, Boolean)] = None
  }

  def ordered(xs: IndexedSeq[Int]): Boolean =
    foldMap(xs.toList, orderedMonoid)(i => Some((i, i, true))).forall(_._3)

  sealed trait WC {
    def countStubWords(stub: String): Int = stub.length min 1
    def toInt: Int
  }
  case class Stub(chars: String) extends WC {
    override def toInt: Int = countStubWords(chars)
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    def toInt: Int = countStubWords(lStub) + words + countStubWords(rStub)
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Part(l1, n1, r1), Part(l2, n2, r2)) =>
        if (r1.isEmpty && l2.isEmpty)
          Part(l1, n1 + n2, r2)
        else
          Part(l1, n1 + n2 + 1, r2)
    }

    override def zero: WC = Stub("")
  }

  def countWords(input: String): Int = {
    println(input)
    def loop(string: String): WC = {
      if (string.length == 0) Stub("")
      else string.splitAt(string.length / 2) match {
        case ("", "") => Stub("")
        case ("", s) => Stub(s)
        case (s, "") => Stub(s)
        case (" ", s) => Part("", 0, s)
        case (s, " ") => Part(s, 0, "")
        case (s1, s2) => wcMonoid.op(loop(s1), loop(s2))
      }
    }

    loop(input).toInt
  }

  def countWords2(input: String ): Int = {
    foldMapV(input.toIndexedSeq, wcMonoid)(c =>
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    ).toInt
  }


  def main(args: Array[String]): Unit = {
    // (((0 - 1) - 2) - 3)
    println(List(1, 2, 3).foldLeft(0)(_ - _))
    println(foldLeftViaFoldMap(List(1, 2, 3), 0)(_ - _))
    // (1 - (2 - (3 - 0)))
    println(List(1, 2, 3).foldRight(0)(_ - _))
    println(foldRightViaFoldMap(List(1, 2, 3), 0)(_ - _))
    println(foldMapV(IndexedSeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), intAddition)(identity))
    val service = Executors.newFixedThreadPool(2)
    println(run(service)(parFoldMap(IndexedSeq.range(0, 100), intAddition)(identity)))
    service.shutdownNow()

    println(ordered(IndexedSeq(1, 2, 3, 4, 5)))
    println(ordered(IndexedSeq(1, 2, 4, 4, 5)))
    println(ordered(IndexedSeq(1, 2, 5, 4, 5)))
    println(countWords("lorem ipsum dolor sit amet, "))
    println(countWords2("lorem ipsum dolor sit amet, "))
  }

}
