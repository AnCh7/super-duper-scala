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

  def foreach[A](tree: Tree[A], f: (A) => Unit): Unit = tree match {
    case EmptyTree =>
    case Node(v, l, r) =>
      f(v)
      foreach(l, f)
      foreach(r, f)
  }

  def foreachRec[A](tree: Tree[A], f: (A) => Unit): Unit = {
    @tailrec
    def iter(f: (A) => Unit, todo: List[Tree[A]]): Unit = todo match {
      case x :: tail => x match {
        case Node(v, l, r) =>
          f(v)
          iter(f, l :: r :: tail)
        case EmptyTree => iter(f, tail)
      }
      case Nil =>
    }
    iter(f, List(tree))
  }

  def reduce[A](tree: Tree[A], value: A, f: (A, A) => A): A = {
    def loop(tree: Tree[A], value: A): A = tree match {
      case Node(v, l, r) => loop(l, f(loop(r, value), v))
      case EmptyTree => value
    }
    loop(tree, value)
  }

  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Node(v, l, r) => Node(f(v), map(l, f), map(r, f))
    case EmptyTree => EmptyTree
  }

  def toList[A](t: Tree[A]): List[A] = t match {
    case Node(v, l, r) => v :: toList(l) ::: toList(r)
    case EmptyTree => List.empty
  }

  def toListRec[A](tree: Tree[A]): List[A] = {
    @tailrec
    def iter(result: List[A], todo: List[Tree[A]]): List[A] = todo match {
      case x :: tail => x match {
        case Node(v, l, r) => iter(v :: result, l :: r :: tail)
        case EmptyTree => iter(result, tail)
      }
      case Nil => result.reverse
    }
    iter(List.empty, List(tree))
  }
}

val tree = Tree(1, Tree(2, Tree(3), Tree(4)), Tree(5, Tree(6), Tree(7)))
Tree.foreach(tree, (x: Int) => println(x))
Tree.foreachRec(tree, (x: Int) => println(x))
Tree.reduce(tree, 0, (x: Int, y: Int) => x + y)
Tree.map(tree, (x: Int) => x + 1)
Tree.toList(tree)
Tree.toListRec(tree)