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

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    inputs.foldRight(State.unit[Machine, (Int, Int)])((i, s) => s.)

    State.unit[Machine, (Int, Int)]((0, 0))
  }



}
