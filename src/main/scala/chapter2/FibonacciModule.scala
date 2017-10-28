package chapter2

object FibonacciModule {
  private def fibonacci(n: Int): Int = {
    if (n <= 1) n
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  private def fibonacciTailRec(n: Int): Int = {
    def loop(n: Int, acc: Int, cur: Int): Int = {
      if (n == 0) acc
      else loop(n - 1, cur, acc + cur)
    }
    loop(n, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fibonacci(7))
    println(fibonacciTailRec(7))
  }
}
