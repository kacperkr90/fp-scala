package chapter2

object FibonacciModule {
  private def fibonacciRec(n: Int): Int = {
    if (n <= 1) n
    else fibonacciRec(n - 1) + fibonacciRec(n - 2)
  }

  private def fibonacciTailRec(n: Int): Int = {
    def loop(n: Int, acc: Int, cur: Int): Int = {
      if (n == 0) acc
      else loop(n - 1, cur, acc + cur)
    }
    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fibonacciRec(7))
    println(fibonacciTailRec(7))
  }
}
