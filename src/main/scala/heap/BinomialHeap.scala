import cats.Order
import cats.syntax.order._
import cats.syntax.option._
import scala.annotation.tailrec
import scala.collection.immutable.Nil

final case class Node[A](rank: Int, value: A, children: List[Node[A]])

final case class BinomialHeap[A](tree: List[Node[A]]) extends AnyVal

object BinomialHeap {

  def empty[A]: BinomialHeap[A] = BinomialHeap(Nil)

  def link[A: Order](x: Node[A], y: Node[A]): Node[A] =
    if (x.value < y.value) {
      Node(x.rank + 1, x.value, y :: x.children)
    } else {
      Node(x.rank + 1, y.value, x :: y.children)
    }

  @tailrec
  def insTree[A: Order](node: Node[A], heap: BinomialHeap[A]): BinomialHeap[A] =
    heap.tree match {
      case Nil => BinomialHeap(List(node))
      case head :: tl =>
        if (node.rank < head.rank) {
          BinomialHeap(node :: heap.tree)
        } else {
          insTree(link(node, head), BinomialHeap(tl))
        }
    }

  def insert[A: Order](value: A, heap: BinomialHeap[A]): BinomialHeap[A] =
    insTree(Node(0, value, Nil), heap)

  def merge[A: Order](
      heap1: BinomialHeap[A],
      heap2: BinomialHeap[A]
  ): BinomialHeap[A] =
    (heap1.tree, heap2.tree) match {
      case (xs, Nil) => BinomialHeap(xs)
      case (Nil, ys) => BinomialHeap(ys)
      case (x :: xs, y :: ys) if x.rank < y.rank =>
        BinomialHeap(x :: merge(BinomialHeap(xs), heap2).tree)
      case (x :: xs, y :: ys) if y.rank < x.rank =>
        BinomialHeap(y :: merge(heap1, BinomialHeap(ys)).tree)
      case (x :: xs, y :: ys) =>
        insTree(link(x, y), merge(BinomialHeap(xs), BinomialHeap(ys)))
    }

  def removeMinTree[A: Order](
      heap: BinomialHeap[A]
  ): Option[(Node[A], BinomialHeap[A])] =
    heap.tree match {
      case Nil      => none
      case x :: Nil => (x, BinomialHeap(List.empty[Node[A]])).some
      case x :: xs =>
        removeMinTree(BinomialHeap(xs)).map {
          case (t, ts) =>
            if (x.value < t.value) {
              (x, BinomialHeap(xs))
            } else {
              (t, BinomialHeap(x :: ts.tree))
            }
        }
    }

  def findMin[A: Order](heap: BinomialHeap[A]): Option[A] = {
    @tailrec
    def go(rest: List[Node[A]], currentMin: Option[A]): Option[A] =
      rest match {
        case Nil => currentMin
        case x :: xs if currentMin.exists(_ < x.value) =>
          go(xs, currentMin)
        case x :: xs =>
          go(xs, x.value.some)
      }
    go(heap.tree, none)
  }

  def deleteMin[A: Order](heap: BinomialHeap[A]): BinomialHeap[A] =
    heap.tree match {
      case Nil => heap
      case _ =>
        removeMinTree(heap)
          .map {
            case (Node(_, _, children), rest) =>
              merge(BinomialHeap(children.reverse), rest)
          }
          .getOrElse(heap)
    }

  implicit val binomialHeapInstance: Heap[BinomialHeap] =
    new Heap[BinomialHeap] {
      def deleteMin[A: Order](heap: BinomialHeap[A]): BinomialHeap[A] =
        BinomialHeap.deleteMin(heap)

      def empty[A]: BinomialHeap[A] = BinomialHeap.empty

      def findMin[A: Order](heap: BinomialHeap[A]): Option[A] =
        BinomialHeap.findMin(heap)

      def insert[A: Order](value: A, heap: BinomialHeap[A]): BinomialHeap[A] =
        BinomialHeap.insert(value, heap)

      def merge[A: Order](
          heap1: BinomialHeap[A],
          heap2: BinomialHeap[A]
      ): BinomialHeap[A] =
        BinomialHeap.merge(heap1, heap2)
    }
}
