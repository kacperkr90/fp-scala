package chapter2

object PartialsModule {

  // f(a, b) => a => g(b, c)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    // f(a, b) => c
    // a => (b => c)
    // a => (b => f(a, b))
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    // f = (a => (b => c))
    // (a => (b => c)) => ((a, b) => c)
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    // f(b) => c
    // g(a) => b
    // h(a) => c
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    def sum = (x: Int, y: Int) => x + y
    def times = (x: Int, y: Int) => x * y
    def sumCurried = curry(sum)

    def timesCurried = curry(times)
    def addTwo = sumCurried(2)

    println(addTwo(5) == 7)

    def sum2 = uncurry(sumCurried)
    println(sum2(2, 5) == 7)

    def timesTwo = timesCurried(2)

    def timesTwoAndAddTwo = compose(addTwo, timesTwo)
    println(timesTwoAndAddTwo(7) == 16)
  }
}
