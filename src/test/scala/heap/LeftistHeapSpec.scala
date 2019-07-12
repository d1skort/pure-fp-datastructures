package heap

import cats.instances.int._
import org.scalacheck.Prop
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers

class LeftistHeapSpec extends FlatSpec with Checkers {
  it should "not violates leftist property" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[LeftistHeap].fromList(list)
      LeftistHeap.rank(LeftistHeap.getLeft(heap)) >= LeftistHeap.rank(LeftistHeap.getRight(heap))
    })
  }
}
