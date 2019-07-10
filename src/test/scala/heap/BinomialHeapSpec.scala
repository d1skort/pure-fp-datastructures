import cats.instances.int._
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{ Prop, Gen }

class BinomialHeapSpec extends FlatSpec with Checkers {
  it should "not violate rank order property" in {
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[BinomialHeap].fromList(list)
      val ranks = heap.tree.map(_.rank)
      ranks == ranks.sorted
    })
  }
}
