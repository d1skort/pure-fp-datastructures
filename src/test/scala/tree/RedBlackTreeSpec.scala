package tree

import cats.instances.int._
import org.scalacheck.Prop
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers
import tree.RedBlackTree.Color.{ Black, Red }
import tree.RedBlackTree.{ Branch, Empty }

class RedBlackTreeSpec extends FlatSpec with Checkers {

  def checkForColor[A](tree: RedBlackTree[A]): Boolean =
    tree match {
      case Branch(Red, Branch(Red, _, _, _), _, _) => false
      case Branch(Red, _, _, Branch(Red, _, _, _)) => false
      case Branch(_, left, _, right)               => checkForColor(left) && checkForColor(right)
      case Empty                                   => true
    }

  def checkForPath[A](tree: RedBlackTree[A]): Boolean = {
    def go(t: RedBlackTree[A]): List[Int] =
      t match {
        case Empty                         => List(0)
        case Branch(Red, left, _, right)   => go(left) ++ go(right)
        case Branch(Black, left, _, right) => (go(left) ++ go(right)).map(_ + 1)
      }

    go(tree) match {
      case Nil     => true
      case x :: xs => xs.forall(_ === x)
    }
  }

  it should "not allow red nodes have red children" in {
    check(Prop.forAll { list: List[Int] =>
      checkForColor(Tree[RedBlackTree].fromList(list))
    })
  }

  it should "contains the same number of black nodes for every path from root to empty node" in {
    check(Prop.forAll { list: List[Int] =>
      checkForPath(Tree[RedBlackTree].fromList(list))
    })
  }
}
