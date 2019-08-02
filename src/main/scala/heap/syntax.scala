package heap

import cats.Order

object syntax {
  implicit final class HeapOps[H[_], A](heap: H[A])(implicit ev: Heap[H]) {
    def merge(that: H[A])(implicit order: Order[A]): H[A] = ev.merge(heap, that)

    def findMin(implicit order: Order[A]): Option[A] = ev.findMin(heap)

    def deleteMin(implicit order: Order[A]): H[A] = ev.deleteMin(heap)

    def insert(value: A)(implicit order: Order[A]): H[A] = ev.insert(value, heap)

    def isEmpty: Boolean = ev.isEmpty(heap)

    def toSortedList(implicit order: Order[A]): List[A] = ev.toSortedList(heap)
  }
}
