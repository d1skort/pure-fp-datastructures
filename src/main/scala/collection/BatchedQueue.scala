package collection

import cats.syntax.option._

final case class BatchedQueue[A](f: List[A], r: List[A])

object BatchedQueue {

  def checkQueueInvariant[A](f: List[A], r: List[A]): BatchedQueue[A] =
    f match {
      case Nil => BatchedQueue(r.reverse, Nil)
      case _   => BatchedQueue(f, r)
    }

  def checkDequeueInvariant[A](f: List[A], r: List[A]): BatchedQueue[A] =
    (f, r) match {
      case (Nil, _) =>
        val mid = r.length / 2
        BatchedQueue(r.drop(mid).reverse, r.take(mid))
      case (_, Nil) =>
        val mid = f.length / 2
        BatchedQueue(f.take(mid), f.drop(mid).reverse)
      case _ => BatchedQueue(f, r)
    }

  implicit val batchedQueue: Queue[BatchedQueue] = new Queue[BatchedQueue] {
    def empty[A]: BatchedQueue[A] = BatchedQueue(Nil, Nil)

    def isEmpty[A](queue: BatchedQueue[A]): Boolean = queue.f.isEmpty

    def snoc[A](queue: BatchedQueue[A], a: A): BatchedQueue[A] =
      checkQueueInvariant(queue.f, a :: queue.r)

    def head[A](queue: BatchedQueue[A]): Option[A] =
      queue.f.headOption

    def tail[A](queue: BatchedQueue[A]): Option[BatchedQueue[A]] =
      queue.f match {
        case Nil     => none
        case _ :: xs => checkQueueInvariant(xs, queue.r).some
      }
  }

  implicit val batchedDequeue: Dequeue[BatchedQueue] = new Dequeue[BatchedQueue] {
    def empty[A]: BatchedQueue[A] = BatchedQueue(Nil, Nil)
    def isEmpty[A](dequeue: BatchedQueue[A]): Boolean = dequeue.f.isEmpty

    def cons[A](a: A, dequeue: BatchedQueue[A]): BatchedQueue[A] =
      checkDequeueInvariant(a :: dequeue.f, dequeue.r)
    def last[A](dequeue: BatchedQueue[A]): Option[A] =
      (dequeue.f, dequeue.r) match {
        case (x :: Nil, Nil) => x.some
        case (_, _)          => dequeue.r.headOption
      }
    def init[A](dequeue: BatchedQueue[A]): Option[BatchedQueue[A]] =
      (dequeue.f, dequeue.r) match {
        case (_ :: _, _ :: ys) => checkDequeueInvariant(dequeue.f, ys).some
        case (_ :: Nil, Nil)   => empty[A].some
        case _                 => none
      }

    def snoc[A](dequeue: BatchedQueue[A], a: A): BatchedQueue[A] =
      checkDequeueInvariant(dequeue.f, a :: dequeue.r)
    def head[A](dequeue: BatchedQueue[A]): Option[A] =
      dequeue.f.headOption
    def tail[A](dequeue: BatchedQueue[A]): Option[BatchedQueue[A]] =
      dequeue.f match {
        case Nil     => none
        case _ :: xs => checkDequeueInvariant(xs, dequeue.r).some
      }
  }
}
