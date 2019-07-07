import WeightBiasedLeftistHeap._
import cats.instances.int._
import cats.syntax.option._
import org.scalatest.FlatSpec
import org.scalatest.OptionValues._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Prop, Gen}

class WeightBiasedLeftistHeapSpec extends FlatSpec with Checkers {
  def safeListMin[A: Ordering](list: List[A]): Option[A] =
    list match {
      case Nil => none
      case _   => list.min.some
    }

  it should "not violates leftist property" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = fromList(list)
      size(getLeft(heap)) >= size(getRight(heap))
    })
  }

  it should "correct return min" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = fromList(list)
      findMin(heap) == safeListMin(list)
    })
  }

  it should "correct insert new item" in {
    check(Prop.forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { list =>
      val heap = fromList(list)
      val newMin = list.min - 1
      findMin(insert(newMin, heap)).value == newMin
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
