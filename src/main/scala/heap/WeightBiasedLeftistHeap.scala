import cats.Order
import cats.syntax.option._
import cats.syntax.order._
import WeightBiasedLeftistHeap.Empty
import WeightBiasedLeftistHeap.Branch

sealed trait WeightBiasedLeftistHeap[+A] extends Product with Serializable
object WeightBiasedLeftistHeap {
  final case class Branch[A](
      value: A,
      left: WeightBiasedLeftistHeap[A],
      right: WeightBiasedLeftistHeap[A],
      size: Long
  ) extends WeightBiasedLeftistHeap[A]

  final case object Empty extends WeightBiasedLeftistHeap[Nothing]

  def size[A](heap: WeightBiasedLeftistHeap[A]): Long = heap match {
    case Branch(_, _, _, size) => size
    case Empty                 => 0
  }

  def getLeft[A](heap: WeightBiasedLeftistHeap[A]): WeightBiasedLeftistHeap[A] =
    heap match {
      case Empty                            => empty
      case Branch(value, left, right, rank) => left
    }

  def getRight[A](
      heap: WeightBiasedLeftistHeap[A]
  ): WeightBiasedLeftistHeap[A] =
    heap match {
      case Empty                            => empty
      case Branch(value, left, right, rank) => right
    }

  def empty[A]: WeightBiasedLeftistHeap[A] = Empty

  def make[A](
      value: A,
      left: WeightBiasedLeftistHeap[A],
      right: WeightBiasedLeftistHeap[A]
  ): WeightBiasedLeftistHeap[A] =
    Branch(value, left, right, size(left) + size(right) + 1)

  def merge[A: Order](
      x: WeightBiasedLeftistHeap[A],
      y: WeightBiasedLeftistHeap[A]
  ): WeightBiasedLeftistHeap[A] =
    (x, y) match {
      case (_, Empty) => x
      case (Empty, _) => y
      case (h1 @ Branch(xValue, xl, xr, _), h2 @ Branch(yValue, yl, yr, _)) =>
        if (xValue < yValue) {
          val rightNewSize = size(h2) + size(xr)
          val leftSize = size(xl)
          val newShoulder = merge(h2, xr)
          if (rightNewSize >= leftSize) {
            make(xValue, newShoulder, xl)
          } else {
            make(xValue, xl, newShoulder)
          }
        } else {
          val rightNewSize = size(h1) + size(yr)
          val leftSize = size(yl)
          val newShoulder = merge(h1, yr)
          if (rightNewSize >= leftSize) {
            make(yValue, newShoulder, yl)
          } else {
            make(yValue, yl, newShoulder)
          }
        }
    }

  def insert[A: Order](
      value: A,
      heap: WeightBiasedLeftistHeap[A]
  ): WeightBiasedLeftistHeap[A] =
    merge(make(value, empty, empty), heap)

  def findMin[A](heap: WeightBiasedLeftistHeap[A]): Option[A] =
    heap match {
      case Empty                  => none
      case Branch(value, _, _, _) => value.some
    }

  def deleteMin[A: Order](
      heap: WeightBiasedLeftistHeap[A]
  ): WeightBiasedLeftistHeap[A] =
    heap match {
      case Empty                     => heap
      case Branch(_, left, right, _) => merge(left, right)
    }

  implicit val weightBiasedHeapInstance: Heap[WeightBiasedLeftistHeap] =
    new Heap[WeightBiasedLeftistHeap] {
      def deleteMin[A: Order](
          heap: WeightBiasedLeftistHeap[A]
      ): WeightBiasedLeftistHeap[A] =
        WeightBiasedLeftistHeap.deleteMin(heap)

      def empty[A]: WeightBiasedLeftistHeap[A] = WeightBiasedLeftistHeap.empty

      def isEmpty[A](heap: WeightBiasedLeftistHeap[A]): Boolean =
        heap match {
          case Empty              => true
          case Branch(_, _, _, _) => false
        }

      def findMin[A: Order](heap: WeightBiasedLeftistHeap[A]): Option[A] =
        WeightBiasedLeftistHeap.findMin(heap)

      def insert[A: Order](
          value: A,
          heap: WeightBiasedLeftistHeap[A]
      ): WeightBiasedLeftistHeap[A] =
        WeightBiasedLeftistHeap.insert(value, heap)

      def merge[A: Order](
          heap1: WeightBiasedLeftistHeap[A],
          heap2: WeightBiasedLeftistHeap[A]
      ): WeightBiasedLeftistHeap[A] =
        WeightBiasedLeftistHeap.merge(heap1, heap2)
    }
}
