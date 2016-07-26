/*
  3) Реализовать следующие функции для работы с бинарным деревом:
    - def reduce[A](t: Tree[A], f: (A, A) => A): A
    - def map[A, B](t: Tree[A], f: A => B): Tree[B]
    - def toList[A](t: Tree[A]): List[A]
  Hint: recursion, pattern matching.
*/

trait Tree[A] {
  def map[B](f: A => B): Tree[B]
  def reduce(f: (A, A) => A): A
  def toList(): List[A]
}

case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {

  override def map[B](f: A => B): Tree[B] = {
    (left, right) match {
      case (null, null) => new Node(f(value), null, null)
      case (l, null) => new Node(f(value), l.map(f), null)
      case (null, r) => new Node(f(value), null, r.map(f))
      case (l, r) => new Node(f(value), l.map(f), r.map(f))
    }
  }

  override def reduce(f: (A, A) => A): A = {
    (left, right) match {
      case (null, null) => value
      case (l, null) => f(value, left.reduce(f))
      case (null, r) => f(value, right.reduce(f))
      case (l, r) => f(value, f(right.reduce(f), right.reduce(f)))
    }
  }

  override def toList(): List[A] = {
    (left, right) match {
      case (null, null) => List(value)
      case (l, null) => List(value) ::: left.toList()
      case (null, r) => List(value) ::: right.toList()
      case (l, r) => List(value) ::: left.toList() ::: right.toList()
    }
  }
}

object Node {
  def apply[A](value: A): Node[A] = Node(value, null, null)
}

val tree =
  Node(1, Node(2, Node(3),
    Node(4)),
    Node(5, Node(6),
      Node(7)))

tree.map(x => x + 1)
tree.reduce((x: Int, y: Int) => x + y)
tree.toList()