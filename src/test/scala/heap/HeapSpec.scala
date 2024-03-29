package heap

import cats.instances.int._
import heap.syntax._
import org.scalacheck.Prop
import org.scalatest.OptionValues._
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.Checkers

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

  def testSortViaHeap[H[_]: Heap] =
    check(Prop.forAll { list: List[Int] =>
      Heap[H].fromList(list).toSortedList === list.sorted
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
      "return correct min for SplayHeap" in {
        testFindMin[SplayHeap]
      }
      "return correct min for PairingHeap" in {
        testFindMin[PairingHeap]
      }
      "return correct min for ExplicitMinHeap for LeftistHeap" in {
        testFindMin[ExplicitMinHeap[LeftistHeap, *]]
      }
      "return correct min for ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testFindMin[ExplicitMinHeap[WeightBiasedLeftistHeap, *]]
      }
      "return correct min for ExplicitMinHeap for BinomialHeap" in {
        testFindMin[ExplicitMinHeap[BinomialHeap, *]]
      }
      "return correct min for ExplicitMinHeap for SplayHeap" in {
        testFindMin[ExplicitMinHeap[SplayHeap, *]]
      }
      "return correct min for ExplicitMinHeap for PairingHeap" in {
        testFindMin[ExplicitMinHeap[PairingHeap, *]]
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
      "correct insert new item for SplayHeap" in {
        testInsertNewItem[SplayHeap]
      }
      "correct insert new item for PairingHeap" in {
        testInsertNewItem[PairingHeap]
      }
      "correct insert new item for ExplicitMinHeap for LeftistHeap" in {
        testInsertNewItem[ExplicitMinHeap[LeftistHeap, *]]
      }
      "correct insert new item for ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testInsertNewItem[ExplicitMinHeap[WeightBiasedLeftistHeap, *]]
      }
      "correct insert new item for ExplicitMinHeap for BinomialHeap" in {
        testInsertNewItem[ExplicitMinHeap[BinomialHeap, *]]
      }
      "correct insert new item for ExplicitMinHeap for SplayHeap" in {
        testInsertNewItem[ExplicitMinHeap[SplayHeap, *]]
      }
      "correct insert new item for ExplicitMinHeap for PairingHeap" in {
        testInsertNewItem[ExplicitMinHeap[PairingHeap, *]]
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
      "correct delete min for SplayHeap" in {
        testDeleteMin[SplayHeap]
      }
      "correct delete min for PairingHeap" in {
        testDeleteMin[PairingHeap]
      }
      "correct delete min for ExplicitMinHeap for LeftistHeap" in {
        testDeleteMin[ExplicitMinHeap[LeftistHeap, *]]
      }
      "correct delete min for ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testDeleteMin[ExplicitMinHeap[WeightBiasedLeftistHeap, *]]
      }
      "correct delete min for ExplicitMinHeap for BinomialHeap" in {
        testDeleteMin[ExplicitMinHeap[BinomialHeap, *]]
      }
      "correct delete min for ExplicitMinHeap for SplayHeap" in {
        testDeleteMin[ExplicitMinHeap[SplayHeap, *]]
      }
      "correct delete min for ExplicitMinHeap for PairingHeap" in {
        testDeleteMin[ExplicitMinHeap[PairingHeap, *]]
      }
    }

    "sort" should {
      "correct sort for LeftistHeap" in {
        testSortViaHeap[LeftistHeap]
      }
      "correct sort via WeightBiasedLeftistHeap" in {
        testSortViaHeap[WeightBiasedLeftistHeap]
      }
      "correct sort via BinomialHeap" in {
        testSortViaHeap[BinomialHeap]
      }
      "correct sort via SplayHeap" in {
        testSortViaHeap[SplayHeap]
      }
      "correct sort via PairingHeap" in {
        testSortViaHeap[PairingHeap]
      }
      "correct sort via ExplicitMinHeap for LeftistHeap" in {
        testSortViaHeap[ExplicitMinHeap[LeftistHeap, *]]
      }
      "correct sort via ExplicitMinHeap for WeightBiasedLeftistHeap" in {
        testSortViaHeap[ExplicitMinHeap[WeightBiasedLeftistHeap, *]]
      }
      "correct sort via ExplicitMinHeap for BinomialHeap" in {
        testSortViaHeap[ExplicitMinHeap[BinomialHeap, *]]
      }
      "correct sort via ExplicitMinHeap for SplayHeap" in {
        testSortViaHeap[ExplicitMinHeap[SplayHeap, *]]
      }
      "correct sort via ExplicitMinHeap for PairingHeap" in {
        testSortViaHeap[ExplicitMinHeap[PairingHeap, *]]
      }
    }
  }
}
