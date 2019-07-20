package collection

import scala.annotation.tailrec

trait Queue[Q[_]] {
  def empty[A]: Q[A]
  def isEmpty[A](queue: Q[A]): Boolean
  def snoc[A](queue: Q[A], a: A): Q[A]
  def head[A](queue: Q[A]): Option[A]
  def tail[A](queue: Q[A]): Option[Q[A]]
}

object Queue {
  def apply[Q[_]](implicit ev: Queue[Q]): Queue[Q] = ev

  object syntax {
    implicit final class QueueOps[Q[_], A](queue: Q[A])(implicit ev: Queue[Q]) {
      def head: Option[A] = ev.head(queue)
      def tail: Option[Q[A]] = ev.tail(queue)
      def snoc(a: A): Q[A] = ev.snoc(queue, a)
      def isEmpty: Boolean = ev.isEmpty(queue)
    }
  }

  def getAllItems[Q[_]: Queue, A](queue: Q[A]): List[A] = {
    import syntax._

    @tailrec
    def go(queue: Q[A], acc: List[A]): List[A] =
      (queue.head, queue.tail) match {
        case (None, None) => acc
        case (Some(x), Some(tail)) =>
          go(tail, x :: acc)
        case _ => acc
      }

    go(queue, Nil).reverse
  }
}
