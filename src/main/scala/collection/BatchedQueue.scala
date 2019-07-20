package collection

import cats.syntax.option._

final case class BatchedQueue[A](f: List[A], r: List[A])

object BatchedQueue {

  def check[A](f: List[A], r: List[A]): BatchedQueue[A] =
    (f, r) match {
      case (Nil, _) => BatchedQueue(r.reverse, Nil)
      case _        => BatchedQueue(f, r)
    }

  implicit val batchedQueue: Queue[BatchedQueue] = new Queue[BatchedQueue] {
    def empty[A]: BatchedQueue[A] = BatchedQueue(Nil, Nil)

    def isEmpty[A](queue: BatchedQueue[A]): Boolean = queue.f.isEmpty

    def snoc[A](queue: BatchedQueue[A], a: A): BatchedQueue[A] =
      check(queue.f, a :: queue.r)

    def head[A](queue: BatchedQueue[A]): Option[A] =
      queue.f.headOption

    def tail[A](queue: BatchedQueue[A]): Option[BatchedQueue[A]] =
      queue.f match {
        case Nil     => none
        case _ :: xs => check(xs, queue.r).some
      }
  }
}
