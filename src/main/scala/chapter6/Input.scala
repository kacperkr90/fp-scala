package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State.sequence(inputs.map(i => State.modify(insertInput()(i))))
        .flatMap(_ => currentState)
  }

  def currentState: State[Machine, (Int, Int)] =
    State(machine => ((machine.coins, machine.candies), machine))

  def insertInput(): Input => Machine => Machine =
    input => machine => (machine, input) match {
      case (Machine(_, c1, c2), _) if c1 <= 0 => machine
      case (Machine(false, c1, c2), Coin) => Machine(true, c1, c2 + 1)
      case (Machine(true, c1, c2), Turn) => Machine(false, c1 - 1, c2)
      case _ => machine
  }

  def main(args: Array[String]): Unit = {
    val machine = Machine(false, 10, 0)
    val state = Machine.simulateMachine(List(Coin, Turn, Coin, Turn))
    val tuple = state.run(machine)
    println(tuple)
  }
}