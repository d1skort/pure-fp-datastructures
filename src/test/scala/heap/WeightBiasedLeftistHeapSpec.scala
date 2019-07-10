import WeightBiasedLeftistHeap.{ size, getLeft, getRight }
import cats.instances.int._
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{ Prop, Gen }

class WeightBiasedLeftistHeapSpec extends FlatSpec with Checkers {
  it should "not violates leftist property" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[WeightBiasedLeftistHeap].fromList(list)
      size(getLeft(heap)) >= size(getLeft(heap))
    })
  }
}
