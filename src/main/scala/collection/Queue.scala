package collection

import scala.annotation.tailrec
import collection.syntax._

trait Queue[Q[_]] {
  def empty[A]: Q[A]
  def isEmpty[A](queue: Q[A]): Boolean
  def snoc[A](queue: Q[A], a: A): Q[A]
  def head[A](queue: Q[A]): Option[A]
  def tail[A](queue: Q[A]): Option[Q[A]]
}

object Queue {
  def apply[Q[_]](implicit ev: Queue[Q]): Queue[Q] = ev

  def toList[Q[_]: Queue, A](queue: Q[A]): List[A] = {
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
