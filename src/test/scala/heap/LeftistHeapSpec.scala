import cats.instances.int._
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{ Prop, Gen }

class LeftistHeapSpec extends FlatSpec with Checkers {
  it should "not violates leftist property" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[LeftistHeap].fromList(list)
      LeftistHeap.rank(LeftistHeap.getLeft(heap)) >= LeftistHeap.rank(LeftistHeap.getRight(heap))
    })
  }
}
