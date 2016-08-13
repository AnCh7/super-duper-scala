import scala.annotation.tailrec

/*
  3) Реализовать следующие функции для работы с бинарным деревом:
    - def reduce[A](t: Tree[A], f: (A, A) => A): A
    - def map[A, B](t: Tree[A], f: A => B): Tree[B]
    - def toList[A](t: Tree[A]): List[A]
  Hint: recursion, pattern matching.

  Update: Выполнено нормально. Большой минус за null-ы. Недочет: не хвостовая рекурсия.
*/

sealed trait Tree[+A]
case object EmptyTree extends Tree[Nothing]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def apply[A]: Tree[A] = EmptyTree
  def apply[A](value: A): Tree[A] = Node(value, EmptyTree, EmptyTree)
  def apply[A](value: A, left: Tree[A], right: Tree[A]): Tree[A] = Node(value, left, right)

  def foreach[A](tree: Tree[A], f: (A) => Unit): Unit = {
    //@tailrec
    def iter[A](tree: Tree[A], f: (A) => Unit): Unit = tree match {
      case EmptyTree =>
      case Node(v, l, r) =>
        iter(l, f)
        f(v)
        iter(r, f)
    }
    iter(tree, f)
  }

  def reduce[A](tree: Tree[A], value: A, f: (A, A) => A): A = {
    //@tailrec
    def loop(tree: Tree[A], value: A): A = tree match {
      case Node(v, l, r) => loop(l, f(loop(r, value), v))
      case EmptyTree => value
    }
    loop(tree, value)
  }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = {
    //@tailrec
    def iter[A](tree: Tree[A], f: A => B): Tree[B] = tree match {
      case Node(v, l, r) => Node(f(v), iter(l, f), iter(r, f))
      case EmptyTree => EmptyTree
    }
    iter(tree, f)
  }

  def toList[A](t: Tree[A]): List[A] = {
    //@tailrec
    def iter[A](t: Tree[A]): List[A] = t match {
      case Node(v, l, r) => v :: iter(l) ::: iter(r)
      case EmptyTree => List.empty
    }
    iter(t)
  }
}

val tree = Tree(1, Tree(2, Tree(3), Tree(4)), Tree(5, Tree(6), Tree(7)))
Tree.foreach(tree, (x: Int) => println(x))
Tree.reduce(tree, 0, (x: Int, y: Int) => x + y)
Tree.map(tree, (x: Int) => x + 1)
Tree.toList(tree)