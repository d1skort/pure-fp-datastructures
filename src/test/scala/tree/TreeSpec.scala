package tree

import cats.instances.int._
import tree.Tree.syntax._
import org.scalacheck.Prop
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.Checkers

class TreeSpec extends WordSpec with Checkers {
  def testMember[T[_]: Tree] =
    check(Prop.forAll { (list: List[Int], i: Int) =>
      val tree = Tree[T].fromList(list)
      tree.member(i) === list.contains(i)
    })

  def testInsert[T[_]: Tree] =
    check(Prop.forAll { (list: List[Int], i: Int) =>
      val tree = Tree[T].fromList(list)
      tree.insert(i).member(i) === true
    })

  "tree" when {
    "member" should {
      "return correct answer for Red Black Tree" in {
        testMember[RedBlackTree]
      }
    }

    "insert" should {
      "correct insert new item for Red Black Tree" in {
        testInsert[RedBlackTree]
      }
    }
  }
}
