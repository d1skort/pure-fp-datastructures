import cats.Order
import cats.syntax.order._
import cats.syntax.option._
import LeftistHeap.Branch
import LeftistHeap.Empty

sealed trait LeftistHeap[+A] extends Product with Serializable
object LeftistHeap {
  final case class Branch[A](value: A, left: LeftistHeap[A], right: LeftistHeap[A], rank: Int)
      extends LeftistHeap[A]

  final case object Empty extends LeftistHeap[Nothing]

  def getLeft[A](heap: LeftistHeap[A]): LeftistHeap[A] =
    heap match {
      case Empty                            => empty
      case Branch(value, left, right, rank) => left
    }

  def getRight[A](heap: LeftistHeap[A]): LeftistHeap[A] =
    heap match {
      case Empty                            => empty
      case Branch(value, left, right, rank) => right
    }

  def empty[A]: LeftistHeap[A] = Empty

  def rank[A](heap: LeftistHeap[A]): Int =
    heap match {
      case Empty                 => 0
      case Branch(_, _, _, rank) => rank
    }

  def make[A](value: A, left: LeftistHeap[A], right: LeftistHeap[A]): LeftistHeap[A] =
    Branch(value, left, right, rank(right) + 1)

  def makeAndSwap[A](value: A, left: LeftistHeap[A], right: LeftistHeap[A]): LeftistHeap[A] =
    if (rank(left) >= rank(right)) {
      make(value, left, right)
    } else {
      make(value, right, left)
    }

  def bubbleUp[A: Order](x: A, left: LeftistHeap[A], right: LeftistHeap[A]): LeftistHeap[A] =
    (left, right) match {
      case (_, Branch(y, yl, yr, _)) if (x > y) =>
        make(y, left, make(x, yl, yr))
      case (Branch(z, zl, zr, _), _) if (x > z) =>
        make(z, make(x, zl, zr), right)
      case _ => make(x, left, right)
    }

  def merge[A: Order](x: LeftistHeap[A], y: LeftistHeap[A]): LeftistHeap[A] =
    (x, y) match {
      case (Empty, _) => y
      case (_, Empty) => x
      case (h1 @ Branch(xValue, xl, xr, _), h2 @ Branch(yValue, yl, yr, _)) =>
        if (xValue < yValue) {
          makeAndSwap(xValue, xl, merge(xr, h2))
        } else {
          makeAndSwap(yValue, yl, merge(yr, h1))
        }
    }

  def insert[A: Order](value: A, heap: LeftistHeap[A]): LeftistHeap[A] =
    heap match {
      case Empty => make(value, empty, empty)
      case Branch(min, left, right, _) =>
        if (rank(left) >= rank(right)) {
          bubbleUp(min, left, insert(value, right))
        } else {
          bubbleUp(min, insert(value, left), right)
        }
    }

  def findMin[A](heap: LeftistHeap[A]): Option[A] =
    heap match {
      case Branch(value, _, _, _) => value.some
      case Empty                  => none
    }

  def deleteMin[A: Order](heap: LeftistHeap[A]): LeftistHeap[A] =
    heap match {
      case Branch(_, left, right, _) => merge(left, right)
      case Empty                     => heap
    }

  implicit val leftistHeapInstance: Heap[LeftistHeap] = new Heap[LeftistHeap] {
    def empty[A]: LeftistHeap[A] = LeftistHeap.empty

    def isEmpty[A](heap: LeftistHeap[A]): Boolean =
      heap match {
        case Empty              => true
        case Branch(_, _, _, _) => false
      }

    def deleteMin[A: Order](heap: LeftistHeap[A]): LeftistHeap[A] =
      LeftistHeap.deleteMin(heap)

    def findMin[A: Order](heap: LeftistHeap[A]): Option[A] =
      LeftistHeap.findMin(heap)

    def insert[A: Order](value: A, heap: LeftistHeap[A]): LeftistHeap[A] =
      LeftistHeap.insert(value, heap)

    def merge[A: Order](heap1: LeftistHeap[A], heap2: LeftistHeap[A]): LeftistHeap[A] =
      LeftistHeap.merge(heap1, heap2)
  }
}
