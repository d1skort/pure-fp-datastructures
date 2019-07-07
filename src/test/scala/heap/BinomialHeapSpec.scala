import BinomialHeap._
import cats.instances.int._
import cats.syntax.option._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalatest.OptionValues._
import org.scalacheck.{Prop, Gen}

class BinomialHeapSpec extends FlatSpec with Checkers {
  def safeListMin[A: Ordering](list: List[A]): Option[A] =
    list match {
      case Nil => none
      case _   => list.min.some
    }

  it should "not violate rank order property" in {
    check(Prop.forAll { list: List[Int] => 
      val heap = fromList(list)
      val ranks = heap.tree.map(_.rank)
      ranks == ranks.sorted
    })
  }

  it should "correct find min" in {
    check(Prop.forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { list =>
      val heap = fromList(list)
      findMin(heap).value == list.min
    })
  }

  it should "correct delete min" in {
    check(Prop.forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { list =>
      val heap = fromList(list.toSet.toList)
      val currentMin = list.min
      findMin(deleteMin(heap)) == safeListMin(list.filterNot(_ == currentMin))
    })
  }
}
