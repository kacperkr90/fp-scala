package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def insertInput(input: Input): (Machine, (Int, Int)) =
    (this, input) match {
      case (Machine(_, c1, c2), _) if c1 <= 0 => (this, (c1, c2))
      case (Machine(false, c1, c2), Coin) => (Machine(true, c1, c2 + 1), (c1, c2 + 1))
      case (Machine(true, c1, c2), Turn) => (Machine(false, c1 - 1, c2), (c1 - 1, c2))
      case (Machine(f, c1, c2), _) => (this, (c1, c2))
    }

}

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs
      .foldRight(Machine.currentState)((i, s) => Machine.withInput(i).map2(s)((x, _) => x))


//    State.unit[Machine, (Int, Int)]((0, 0))
  }

  def currentState: State[Machine, (Int, Int)] =
    State(machine => ((machine.coins, machine.candies), machine))

  def withInput: Input => State[Machine, (Int, Int)] =
    input => State(machine => (machine, input) match {
      case (Machine(_, c1, c2), _) if c1 <= 0 => ((c1, c2), machine)
      case (Machine(false, c1, c2), Coin) => ((c1, c2 + 1), Machine(true, c1, c2 + 1))
      case (Machine(true, c1, c2), Turn) => ((c1 - 1, c2), Machine(false, c1 - 1, c2))
      case (Machine(f, c1, c2), _) => ((c1, c2), machine)
    })

  def main(args: Array[String]): Unit = {
    val machine = Machine(false, 10, 0)
    val state = Machine.simulateMachine(List(Coin, Turn, Coin, Turn))
    val tuple = state.run(machine)
    println(tuple)
  }
}