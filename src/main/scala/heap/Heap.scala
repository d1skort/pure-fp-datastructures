package heap

import cats.Order

import scala.annotation.tailrec

trait Heap[H[_]] {
  def empty[A]: H[A]

  def isEmpty[A](heap: H[A]): Boolean

  def merge[A: Order](heap1: H[A], heap2: H[A]): H[A]

  def findMin[A: Order](heap: H[A]): Option[A]

  def deleteMin[A: Order](heap: H[A]): H[A]

  def insert[A: Order](value: A, heap: H[A]): H[A]

  def fromList[A: Order](list: List[A]): H[A] =
    list match {
      case Nil      => empty
      case x :: Nil => insert(x, empty)
      case _ =>
        val middle = list.length / 2
        val (left, right) = list.splitAt(middle)
        merge(fromList(left), fromList(right))
    }

  def toSortedList[A: Order](heap: H[A]): List[A] = {
    @tailrec
    def go(heap: H[A], res: List[A]): List[A] =
      findMin(heap) match {
        case None        => res
        case Some(value) => go(deleteMin(heap), value :: res)
      }

    go(heap, Nil).reverse
  }
}

object Heap {
  def apply[H[_]](implicit ev: Heap[H]): Heap[H] = ev

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
}
