import cats.Order
import cats.syntax.order._
import cats.syntax.option._
import BinomialHeap.{
  insert => innerInsert,
  merge => innerMerge,
  deleteMin => innerDelete,
  findMin => innerFindMin
  // fromList => innerFromList
}

final case class ExplicitMinBinomialHeap[A](
    min: Option[A],
    heap: BinomialHeap[A]
)

object ExplicitMinBinomialHeap {
  private def min[A: Order](a: Option[A], b: Option[A]): Option[A] =
    (a, b) match {
      case (Some(aa), Some(bb)) if (aa <= bb) => a
      case (Some(aa), Some(bb)) if (aa > bb)  => b
      case _                                  => a.orElse(b)
    }

  def findMin[A](heap: ExplicitMinBinomialHeap[A]): Option[A] =
    heap.min

  def insert[A: Order](
      value: A,
      heap: ExplicitMinBinomialHeap[A]
  ): ExplicitMinBinomialHeap[A] =
    ExplicitMinBinomialHeap(
      min(value.some, heap.min),
      innerInsert(value, heap.heap)
    )

  def merge[A: Order](
      heap1: ExplicitMinBinomialHeap[A],
      heap2: ExplicitMinBinomialHeap[A]
  ): ExplicitMinBinomialHeap[A] =
    ExplicitMinBinomialHeap(
      min(heap1.min, heap2.min),
      innerMerge(heap1.heap, heap2.heap)
    )

  def deleteMin[A: Order](
      heap: ExplicitMinBinomialHeap[A]
  ): ExplicitMinBinomialHeap[A] = {
    val newHeap = innerDelete(heap.heap)
    ExplicitMinBinomialHeap(innerFindMin(newHeap), newHeap)
  }

  // def fromList[A: Order](list: List[A]): ExplicitMinBinomialHeap[A] = {
  //   val heap = innerFromList(list)
  //   ExplicitMinBinomialHeap(innerFindMin(heap), heap)
  // }
}
