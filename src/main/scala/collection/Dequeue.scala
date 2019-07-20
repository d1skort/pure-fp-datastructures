package collection

trait Dequeue[D[_]] extends Queue[D] {
  def cons[A](a: A, deque: D[A]): D[A]
  def last[A](dequeue: D[A]): Option[A]
  def init[A](deque: D[A]): Option[D[A]]
}

object Dequeue {
  def apply[D[_]](implicit ev: Dequeue[D]): Dequeue[D] = ev

  object syntax {
    implicit final class DequeOps[D[_], A](dequeue: D[A])(implicit ev: Dequeue[D]) {
      def isEmpty: Boolean = ev.isEmpty(dequeue)
      def snoc(a: A): D[A] = ev.snoc(dequeue, a)
      def cons(a: A): D[A] = ev.cons(a, dequeue)
      def tail: Option[D[A]] = ev.tail(dequeue)
      def init: Option[D[A]] = ev.init(dequeue)
    }
  }
}
