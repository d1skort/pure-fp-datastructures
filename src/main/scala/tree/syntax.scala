package tree

import cats.Order

object syntax {
  implicit final class TreeOps[T[_], A: Order](t: T[A])(implicit ev: Tree[T]) {
    def member(a: A): Boolean = ev.member(a, t)
    def insert(a: A): T[A] = ev.insert(a, t)
  }
}
