package heap

import cats.Order
import cats.syntax.order._
import cats.syntax.option._

sealed trait PairingHeap[+A] extends Product with Serializable

object PairingHeap {
  final case object Empty extends PairingHeap[Nothing]
  final case class T[A](elem: A, tree: List[PairingHeap[A]]) extends PairingHeap[A]

  implicit val pairingHeap: Heap[PairingHeap] = new Heap[PairingHeap] {
    def empty[A]: PairingHeap[A] = Empty

    def isEmpty[A](heap: PairingHeap[A]): Boolean =
      heap match {
        case Empty => true
        case _     => false
      }

    def merge[A: Order](heap1: PairingHeap[A], heap2: PairingHeap[A]): PairingHeap[A] =
      (heap1, heap2) match {
        case (Empty, _) => heap2
        case (_, Empty) => heap1
        case (h1 @ T(x, hs1), h2 @ T(y, hs2)) =>
          if (x < y) {
            T(x, h2 :: hs1)
          } else {
            T(y, h1 :: hs2)
          }
      }

    def findMin[A: Order](heap: PairingHeap[A]): Option[A] =
      heap match {
        case Empty          => none
        case T(minValue, _) => minValue.some
      }

    def deleteMin[A: Order](heap: PairingHeap[A]): PairingHeap[A] =
      heap match {
        case Empty    => heap
        case T(_, hs) => mergePairs(hs)
      }

    def insert[A: Order](value: A, heap: PairingHeap[A]): PairingHeap[A] =
      merge(T(value, Nil), heap)

    private def mergePairs[A: Order](list: List[PairingHeap[A]]): PairingHeap[A] =
      list match {
        case Nil            => Empty
        case x :: Nil       => x
        case h1 :: h2 :: hs => merge(merge(h1, h2), mergePairs(hs))
      }
  }
}
