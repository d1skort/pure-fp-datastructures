package heap

import cats.Order
import cats.syntax.option._
import cats.syntax.order._
import heap.syntax._

final case class ExplicitMinHeap[H[_], A](min: Option[A], innerHeap: H[A])

object ExplicitMinHeap {
  def min[A: Order](a: Option[A], b: Option[A]): Option[A] =
    (a, b) match {
      case (Some(aa), Some(bb)) if aa <= bb => a
      case (Some(aa), Some(bb)) if aa > bb  => b
      case _                                => a.orElse(b)
    }

  implicit def explicitMinHeapInstance[H[_]: Heap]: Heap[ExplicitMinHeap[H, *]] =
    new Heap[ExplicitMinHeap[H, *]] {
      def findMin[A: Order](heap: ExplicitMinHeap[H, A]): Option[A] = heap.min

      def insert[A: Order](value: A, heap: ExplicitMinHeap[H, A]): ExplicitMinHeap[H, A] =
        ExplicitMinHeap(min(value.some, heap.min), heap.innerHeap.insert(value))

      def merge[A: Order](heap1: ExplicitMinHeap[H, A],
                          heap2: ExplicitMinHeap[H, A]): ExplicitMinHeap[H, A] =
        ExplicitMinHeap(min(heap1.min, heap2.min), heap1.innerHeap.merge(heap2.innerHeap))

      def deleteMin[A: Order](heap: ExplicitMinHeap[H, A]): ExplicitMinHeap[H, A] = {
        val newHeap = heap.innerHeap.deleteMin
        ExplicitMinHeap(newHeap.findMin, newHeap)
      }

      def empty[A]: ExplicitMinHeap[H, A] = ExplicitMinHeap(none, Heap[H].empty)

      def isEmpty[A](heap: ExplicitMinHeap[H, A]): Boolean =
        heap.innerHeap.isEmpty
    }
}
