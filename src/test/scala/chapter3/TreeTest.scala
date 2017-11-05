package chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeTest extends FlatSpec with Matchers {
  val tree = Branch(
    Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Leaf(9)
    ),
    Branch(
      Branch(
        Leaf(4),
        Leaf(5)
      ),
      Branch(
        Leaf(6),
        Branch(
          Leaf(7),
          Leaf(8)
        )
      )
    )
  )

  Tree.size(tree) should equal (15)

  Tree.maximum(tree) should equal (9)
  Tree.maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))) should equal (2)

  Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))) should equal (3)
  Tree.depth(tree) should equal (5)

  Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))))(_ * 2) should equal (Branch(Leaf(2), Branch(Leaf(4), Leaf(4))))

  Tree.sizeViaFold(tree) should equal (15)

  Tree.maximumViaFold(tree) should equal (9)
  Tree.maximumViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))) should equal (2)

  Tree.depthViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(2)))) should equal (3)
  Tree.depthViaFold(tree) should equal (5)

  Tree.mapViaFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(2))))(_ * 2) should equal (Branch(Leaf(2), Branch(Leaf(4), Leaf(4))))
}
