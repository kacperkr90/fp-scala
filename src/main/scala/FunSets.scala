object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = _ == elem


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set =
    n => s(n) || t(n)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set =
    n => s(n) && t(n)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set =
    n => s(n) && !t(n)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set =
    intersect(s, p)


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  def fold[A](s: Set, acc: A)(f: (Int, A) => A): A = {
    def go(a: Int, acc: A): A = {
      if (a > bound) acc
      else if (!s(a)) go(a + 1, acc)
      else go(a + 1, f(a, acc))
    }
    go(-bound, acc)
  }

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(s, a) && !p(a)) false
      else if (a > bound) true
      else iter(a + 1)
    }
    iter(-bound)
  }

  def forall2(s: Set, p: Int => Boolean): Boolean =
    fold(s, true)((n, acc) => p(n) && acc)

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean =
    fold(s, false)((n, acc) => p(n) || acc)

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set =
    s.compose(nn => f(nn))

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def toList(s: Set): List[Int] = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.toList
  }


  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }

  def main(args: Array[String]): Unit = {
    val mod2: Set = _ % 2 == 0
    val mod4: Set = _ % 4 == 0
    val mod5: Set = _ % 5 == 0

    printSet(singletonSet(5))
    printSet(union(mod2, mod5))
    printSet(intersect(mod2, mod5))
    printSet(diff(mod5, mod2))
    printSet(filter(mod5, _ % 4 == 0))
    printSet(map(mod5, _ + 1))

    println(forall(mod4, _ % 3 == 0))
    println(forall(mod4, _ % 2 == 0))

    println(forall2(mod4, _ % 3 == 0))
    println(forall2(mod4, _ % 2 == 0))

    println(exists(mod4, _ % 2 == 0))
    println(exists(mod4, _ % 3 == 0))

  }
}