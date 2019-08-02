package tree

import cats.Order

trait Tree[T[_]] {
  def empty[A]: T[A]
  def insert[A: Order](a: A, t: T[A]): T[A]
  def member[A: Order](a: A, t: T[A]): Boolean
  def fromList[A: Order](list: List[A]): T[A] =
    list.foldRight(empty[A]) {
      case (a, t) =>
        insert(a, t)
    }
}

object Tree {
  def apply[T[_]](implicit ev: Tree[T]): Tree[T] = ev
}
