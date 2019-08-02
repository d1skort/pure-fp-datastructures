package collection

trait Dequeue[D[_]] extends Queue[D] {
  def cons[A](a: A, deque: D[A]): D[A]
  def last[A](dequeue: D[A]): Option[A]
  def init[A](deque: D[A]): Option[D[A]]
}

object Dequeue {
  def apply[D[_]](implicit ev: Dequeue[D]): Dequeue[D] = ev
}
