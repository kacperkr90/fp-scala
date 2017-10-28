package chapter2

object ArraysSortingModule {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      // CORNER CASES
      // if n >= than as.length - 1 then true
      // if n == 0 then call loop(n + 1)
      // if as(n) is < than as(n - 1) then false
      // if as(n) is > than as(n - 1) then call loop(n + 1)
      if (n >= as.length) true
      else if (n == 0 || ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val array = Array(1, 2, 3, 8, 10)
    val array2 = Array(1, 2, 3, 11, 10)
    val array3 = Array(1)
    val array4 = Array("ala", "ma", "kota")
    println(isSorted(array, (x: Int, y: Int) => x <= y))
    println(isSorted(array2, (x: Int, y: Int) => x <= y))
    println(isSorted(array3, (x: Int, y: Int) => x <= y))
    println(isSorted(array4, (x: String, y: String) => x <= y))
  }
}
