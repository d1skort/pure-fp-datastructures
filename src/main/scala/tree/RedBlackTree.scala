package tree

import cats.Order
import cats.syntax.order._

import scala.annotation.tailrec

sealed trait RedBlackTree[+A] extends Product with Serializable
object RedBlackTree {
  sealed trait Color extends Product with Serializable
  object Color {
    final case object Red extends Color
    final case object Black extends Color
  }

  final case class Branch[A](color: Color, left: RedBlackTree[A], value: A, right: RedBlackTree[A])
      extends RedBlackTree[A]
  final case object Empty extends RedBlackTree[Nothing]

  def balance[A](color: Color,
                 left: RedBlackTree[A],
                 root: A,
                 right: RedBlackTree[A]): RedBlackTree[A] = ???

  def ins[A: Order](value: A, tree: RedBlackTree[A]): RedBlackTree[A] =
    tree match {
      case Empty => Branch(Color.Red, Empty, value, Empty)
      case Branch(color, left, root, right) if value < root =>
        balance(color, ins(value, left), root, right)
      case Branch(color, left, root, right) if value > root =>
        balance(color, left, root, ins(value, right))
      case _ => tree
    }

  implicit val redBlackTreeInstance: Tree[RedBlackTree] = new Tree[RedBlackTree] {
    def empty[A]: RedBlackTree[A] = Empty

    def insert[A: Order](a: A, t: RedBlackTree[A]): RedBlackTree[A] = {
      val Branch(_, left, y, right) = ins(a, t)
      Branch(Color.Black, left, y, right)
    }

    @tailrec
    def member[A: Order](a: A, t: RedBlackTree[A]): Boolean =
      t match {
        case Empty                                   => false
        case Branch(_, left, value, _) if a < value  => member(a, left)
        case Branch(_, _, value, right) if a > value => member(a, right)
        case _                                       => true
      }
  }
}
