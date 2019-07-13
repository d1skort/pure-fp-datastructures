package tree

import cats.instances.int._
import org.scalacheck.Prop
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers
import tree.RedBlackTree.Color.Red
import tree.RedBlackTree.{ Branch, Empty }

class RedBlackTreeSpec extends FlatSpec with Checkers {

  def checkForColor[A](tree: RedBlackTree[A]): Boolean =
    tree match {
      case Branch(Red, Branch(Red, _, _, _), _, _) => false
      case Branch(Red, _, _, Branch(Red, _, _, _)) => false
      case Branch(_, left, _, right)               => checkForColor(left) && checkForColor(right)
      case Empty                                   => true
    }

  it should "not allow for red children be with red parent" in {
    check(Prop.forAll { list: List[Int] =>
      checkForColor(Tree[RedBlackTree].fromList(list))
    })
  }
}
