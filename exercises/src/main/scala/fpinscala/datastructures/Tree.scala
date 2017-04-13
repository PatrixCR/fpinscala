package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }

  def maximum(tree: Tree[Int]): Int = {
    def max(tree: Tree[Int], currentMax: Int): Int =
      tree match {
        case Leaf(a) => a.max(currentMax)
        case Branch(l, r) => max(l, currentMax).max(max(r, currentMax))
      }

    max(tree, Int.MinValue)
  }
}