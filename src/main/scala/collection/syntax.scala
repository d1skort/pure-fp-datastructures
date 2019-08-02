package collection

object syntax {
  implicit final class QueueOps[Q[_], A](queue: Q[A])(implicit ev: Queue[Q]) {
    def head: Option[A] = ev.head(queue)
    def tail: Option[Q[A]] = ev.tail(queue)
    def snoc(a: A): Q[A] = ev.snoc(queue, a)
    def isEmpty: Boolean = ev.isEmpty(queue)
  }

  implicit final class DequeOps[D[_], A](dequeue: D[A])(implicit ev: Dequeue[D]) {
    def cons(a: A): D[A] = ev.cons(a, dequeue)
    def init: Option[D[A]] = ev.init(dequeue)
    def last: Option[A] = ev.last(dequeue)
  }
}
