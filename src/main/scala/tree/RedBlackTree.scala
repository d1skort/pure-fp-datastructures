package tree

import cats.Order
import cats.syntax.order._
import cats.syntax.option._
import tree.RedBlackTree.Color.{ Black, Red }

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
                 right: RedBlackTree[A]): RedBlackTree[A] =
    (color, left, root, right) match {
      case (Black, Branch(Red, Branch(Red, a, x, b), y, c), z, d) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case (Black, Branch(Red, a, x, Branch(Red, b, y, c)), z, d) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case (Black, a, x, Branch(Red, b, y, Branch(Red, c, z, d))) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case (Black, a, x, Branch(Red, Branch(Red, b, y, c), z, d)) =>
        Branch(Red, Branch(Black, a, x, b), y, Branch(Black, c, z, d))
      case _ => Branch(color, left, root, right)
    }

  def ins[A: Order](value: A, tree: RedBlackTree[A]): RedBlackTree[A] =
    tree match {
      case Empty => Branch(Red, Empty, value, Empty)
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

    def member[A: Order](a: A, t: RedBlackTree[A]): Boolean = {
      @tailrec
      def go(t: RedBlackTree[A], acc: Option[A]): Boolean =
        t match {
          case Branch(_, left, root, _) if a < root => go(left, acc)
          case Branch(_, _, root, right)            => go(right, root.some)
          case Empty                                => acc.contains(a)
        }

      go(t, none)
    }

  }
}
