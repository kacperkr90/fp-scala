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

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)

    println(List.tail(list) == List(2, 3, 4, 5))

    println(List.setHead(list, 5) == List(5, 2, 3, 4, 5))

    println(List.drop(list, 3) == List(4, 5))
    println(List.drop(list, 10) == Nil)
    println(List.drop(list, 0) == list)

    println(List.dropWhile(list, (x: Int) => x < 4) == List(4, 5))
    println(List.dropWhile(list, (x: Int) => x < 0) == list)
    println(List.dropWhile(list, (x: Int) => x > 0) == Nil)

    println(init(list) == List(1, 2, 3, 4))
    println(init(List(1)) == Nil)
  }
}
