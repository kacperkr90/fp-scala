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
  }
}
