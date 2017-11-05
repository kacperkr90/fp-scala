package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case _ => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l: Tree[Int], r: Tree[Int]) => maximum(l).max(maximum(r))
    case Leaf(a) => a
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + depth(l).max(depth(r))
    case _ => 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(a) => Leaf(f(a))
  }

  def fold[A, B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match {
    case Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(a) => g(a)
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)((x: Int, y: Int) => 1 + x + y)(_ => 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)((x: Int, y: Int) => x.max(y))((x: Int) => x)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)((x: Int, y: Int) => 1 + x.max(y))(_ => 1)

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((x:Tree[B], y:Tree[B]) => Branch(x, y): Tree[B])((x:A) => Leaf(f(x)))
}