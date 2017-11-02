package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def head[A](as: List[A]): A = as match {
    case Nil => throw new RuntimeException("empty list")
    case Cons(x, _) => x
  }

  def setHead[A](as: List[A], h: A) =
    Cons(h, List.tail(as))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (l == Nil) l
    else if (n > 0) drop(List.tail(l), n - 1)
    else l

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
    case _ => Nil
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => y + 1)

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }


  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((xs, h) => Cons(h, xs))

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(b,a)): B)(z)
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def appendViaFoldRight[A](as: List[A], z: List[A]): List[A] =
    foldRight(as, z)(Cons(_, _))

  def appendViaFoldLeft[A](as: List[A], z: List[A]): List[A] =
    foldLeft(reverse(as), z)((acc, x) => Cons(x, acc))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((xs, acc) => appendViaFoldRight(xs, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, Nil:List[B])((x, xs) => Cons(f(x), xs))

  def add1ToEach(l: List[Int]) =
    map(l)(_ + 1)

  def doublesToStrings(l: List[Double]) =
    map(l)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)

  def trace[A](s: String, a: A): A = {
    println(s)
    a
  }

}
