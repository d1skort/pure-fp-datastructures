package collection

import collection.syntax._
import org.scalacheck.Prop
import org.scalatest.{ Matchers, WordSpec }
import org.scalatestplus.scalacheck.Checkers

class DequeueSpec extends WordSpec with Matchers with Checkers {
  def testIsEmpty[D[_]: Dequeue] =
    Dequeue[D].empty.isEmpty shouldBe true

  def testSupportFIFO[D[_]: Dequeue] =
    check(Prop.forAll { list: List[Int] =>
      val dequeue = list.foldLeft(Dequeue[D].empty[Int]) {
        case (d, i) =>
          d.snoc(i)
      }
      Queue.toList(dequeue) === list
    })

  def testSupportLIFO[D[_]: Dequeue] =
    check(Prop.forAll { list: List[Int] =>
      val dequeue = list.foldLeft(Dequeue[D].empty[Int]) {
        case (d, i) =>
          d.cons(i)
      }
      Queue.toList(dequeue) === list.reverse
    })

  def testRemoveFirstElement[D[_]: Dequeue] =
    check(Prop.forAll { list: List[Int] =>
      val dequeue = list.foldLeft(Dequeue[D].empty[Int]) {
        case (d, i) => d.snoc(i)
      }
      val listTail = if (list.isEmpty) {
        None
      } else {
        Some(list.tail)
      }
      dequeue.tail.map(d => Queue.toList(d)) === listTail
    })

  def testRemoveLastElement[D[_]: Dequeue] =
    check(Prop.forAll { list: List[Int] =>
      val dequeue = list.foldLeft(Dequeue[D].empty[Int]) {
        case (d, i) => d.snoc(i)
      }
      val listInit = if (list.isEmpty) {
        None
      } else {
        Some(list.init)
      }
      dequeue.init.map(d => Queue.toList(d)) === listInit
    })

  "dequeue" when {
    "empty" should {
      "return true for empty BatchedQueue" in {
        testIsEmpty[BatchedQueue]
      }
    }

    "not empty" should {
      "support FIFO invariant for BatchedQueue" in {
        testSupportFIFO[BatchedQueue]
      }

      "support LIFO invariant for BatchedQueue" in {
        testSupportLIFO[BatchedQueue]
      }

      "correct remove first element" in {
        testRemoveFirstElement[BatchedQueue]
      }

      "correct remove last element" in {
        testRemoveLastElement[BatchedQueue]
      }
    }
  }
}
