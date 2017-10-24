object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(n: Int) : String = {
    val msg = "The absolute value of %d is %d"
    msg.format(n, abs(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }

}
