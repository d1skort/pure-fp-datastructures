package heap

import cats.Order
import cats.syntax.order._
import cats.syntax.option._

import scala.annotation.tailrec

sealed trait SplayHeap[+A] extends Product with Serializable

object SplayHeap {
  final case class Branch[A](left: SplayHeap[A], value: A, right: SplayHeap[A]) extends SplayHeap[A]
  final case object Empty extends SplayHeap[Nothing]

  def partition[A: Order](pivot: A, heap: SplayHeap[A]): (SplayHeap[A], SplayHeap[A]) =
    heap match {
      case Empty => (Empty, Empty)
      case Branch(a, x, b) if x <= pivot =>
        b match {
          case Empty => (heap, Empty)
          case Branch(a1, y, b1) if y <= pivot =>
            val (small, big) = partition(pivot, b1)
            (Branch(Branch(a, x, a1), y, small), big)
          case Branch(a1, y, b1) =>
            val (small, big) = partition(pivot, a1)
            (Branch(a, x, small), Branch(big, y, b1))
        }
      case Branch(a, x, b) =>
        a match {
          case Empty => (Empty, heap)
          case Branch(a1, y, b1) if y <= pivot =>
            val (small, big) = partition(pivot, b1)
            (Branch(a1, y, small), Branch(big, x, b))
          case Branch(a1, y, b1) =>
            val (small, big) = partition(pivot, a1)
            (small, Branch(big, y, Branch(b1, x, b)))
        }
    }

  implicit val splayHeap: Heap[SplayHeap] = new Heap[SplayHeap] {
    def empty[A]: SplayHeap[A] = Empty

    def isEmpty[A](heap: SplayHeap[A]): Boolean = heap match {
      case Empty => true
      case _     => false
    }

    def merge[A: Order](x: SplayHeap[A], y: SplayHeap[A]): SplayHeap[A] =
      (x, y) match {
        case (Empty, _) => y
        case (Branch(a, root, b), _) =>
          val (small, big) = partition(root, y)
          Branch(merge(small, a), root, merge(big, b))
      }

    @tailrec
    def findMin[A: Order](heap: SplayHeap[A]): Option[A] =
      heap match {
        case Empty               => none
        case Branch(Empty, x, _) => x.some
        case Branch(a, _, _)     => findMin(a)
      }

    def deleteMin[A: Order](heap: SplayHeap[A]): SplayHeap[A] =
      heap match {
        case Empty                             => Empty
        case Branch(Empty, _, b)               => b
        case Branch(Branch(Empty, _, b), y, c) => Branch(b, y, c)
        case Branch(Branch(a, x, b), y, c)     => Branch(deleteMin(a), x, Branch(b, y, c))
      }

    def insert[A: Order](value: A, heap: SplayHeap[A]): SplayHeap[A] = {
      val (smaller, bigger) = partition(value, heap)
      Branch(smaller, value, bigger)
    }
  }
}
