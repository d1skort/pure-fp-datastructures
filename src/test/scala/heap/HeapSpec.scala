import Heap.syntax._
import cats.instances.int._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{ Prop, Gen }
import org.scalatest.WordSpec
import org.scalatest.OptionValues._

class HeapSpec extends WordSpec with Checkers {

  def testFindMin[H[_]: Heap] =
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[H].fromList(list)
      heap.findMin === list.minOption
    })

  def testInsertNewItem[H[_]: Heap] =
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[H].fromList(list.filter(_ >= 0))
      heap.insert(-1).findMin.value === -1
    })

  def testDeleteMin[H[_]: Heap] =
    check(Prop.forAll { list: List[Int] =>
      val heap = Heap[H].fromList(list.toSet.toList)
      val currentMin = list.minOption.getOrElse(42)
      heap.deleteMin.findMin === list.filterNot(_ == currentMin).minOption
    })

  "heap" when {
    "findMin" should {
      "return correct min for LeftistHeap" in {
        testFindMin[LeftistHeap]
      }
      "return correct min for WeightBiasedLeftistHeap" in {
        testFindMin[WeightBiasedLeftistHeap]
      }
      "return correct min for BinomialHeap" in {
        testFindMin[BinomialHeap]
      }
      "return correct min for ExplicitMinHeap for LeftistHeap" in {
        testFindMin[ExplicitMinHeap[LeftistHeap, ?]]
      }
      "return correct min for ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testFindMin[ExplicitMinHeap[WeightBiasedLeftistHeap, ?]]
      }
      "return correct min for ExplicitMinHeap for BinomialHeap" in {
        testFindMin[ExplicitMinHeap[BinomialHeap, ?]]
      }
    }

    "insert" should {
      "correct insert new item for LeftistHeap" in {
        testInsertNewItem[LeftistHeap]
      }
      "correct insert new item for WeightBiasedLeftistHeap" in {
        testInsertNewItem[WeightBiasedLeftistHeap]
      }
      "correct insert new item for BinomialHeap" in {
        testInsertNewItem[BinomialHeap]
      }
      "correct insert new item for ExplicitMinHeap for LeftistHeap" in {
        testInsertNewItem[ExplicitMinHeap[LeftistHeap, ?]]
      }
      "correct insert new item for ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testInsertNewItem[ExplicitMinHeap[WeightBiasedLeftistHeap, ?]]
      }
      "correct insert new item for ExplicitMinHeap for BinomialHeap" in {
        testInsertNewItem[ExplicitMinHeap[BinomialHeap, ?]]
      }
    }

    "deleteMin" should {
      "correct delete min for LeftistHeap" in {
        testDeleteMin[LeftistHeap]
      }
      "correct delete min for WeightBiasedLeftistHeap" in {
        testDeleteMin[WeightBiasedLeftistHeap]
      }
      "correct delete min for BinomialHeap" in {
        testDeleteMin[BinomialHeap]
      }
      "correct delete min for ExplicitMinHeap for LeftistHeap" in {
        testDeleteMin[ExplicitMinHeap[LeftistHeap, ?]]
      }
      "correct delete min for ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testDeleteMin[ExplicitMinHeap[WeightBiasedLeftistHeap, ?]]
      }
      "correct delete min for ExplicitMinHeap for BinomialHeap" in {
        testDeleteMin[ExplicitMinHeap[BinomialHeap, ?]]
      }
    }
  }
}