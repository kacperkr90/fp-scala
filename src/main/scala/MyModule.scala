object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)
    go(n, 1)
  }

  def changesCount(amount: Int, denominations: List[Int]): Int = {
    def go(am: Int, de: List[Int]): Int =
      (am, de) match {
        case (a, List()) if a > 0 => 0
        case (0, _) => 1
        case (a, ns) if a >= ns.max => go(a - ns.max, ns) + go(a, ns.tail)
        case (a, ns) => go(a, ns.tail)
      }
    go(amount, denominations.sortWith(_ > _))
  }

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s value of d% is %d."
    msg.format(name, n, f(n))
  }

  private def formatAbs(n: Int) : String = {
    formatResult("absolute", n, abs)
  }

  private def formatFactorial(n: Int): String = {
    formatResult("factorial", n, factorial)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(changesCount(49, List(10,15,11,12,20)))
  }
}
