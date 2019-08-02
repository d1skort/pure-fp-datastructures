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

  def fromList[A: Order](list: List[A]): H[A] = {
    @tailrec
    def go(heap: H[A], rest: List[A]): H[A] =
      rest match {
        case Nil     => heap
        case x :: xs => go(insert(x, heap), xs)
      }

    go(empty, list)
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
}
