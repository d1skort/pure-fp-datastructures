package heap

import cats.instances.int._
import heap.WeightBiasedLeftistHeap.{ getLeft, getRight, size }
import org.scalacheck.Prop
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers

class WeightBiasedLeftistHeapSpec extends FlatSpec with Checkers {
  it should "not violates leftist property" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[WeightBiasedLeftistHeap].fromList(list)
      size(getLeft(heap)) >= size(getRight(heap))
    })
  }
}
