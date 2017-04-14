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

  def depth(tree: Tree[Int]): Int = {
    def depth(tree: Tree[Int], currentDepth: Int): Int =
      tree match {
        case Leaf(_) => currentDepth + 1
        case Branch(l, r) => depth(l, currentDepth + 1).max(depth(r, currentDepth + 1))
      }

    depth(tree, 0)
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = {
    t match {
      case Leaf(a) => l(a)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((left, right) => left + right + 1)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(identity)((left, right) => left.max(right))

  def depth2(tree: Tree[Int]): Int = fold(tree)(_ => 1)((left, right) => left.max(right) + 1)

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)):Tree[B])((left, right) =>  Branch(left, right))
}