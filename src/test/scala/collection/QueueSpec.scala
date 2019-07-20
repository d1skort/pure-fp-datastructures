package collection

import collection.Queue.syntax._
import org.scalacheck.Prop
import org.scalatest.{ Matchers, WordSpec }
import org.scalatestplus.scalacheck.Checkers

class QueueSpec extends WordSpec with Matchers with Checkers {
  def testIsEmpty[Q[_]: Queue] =
    Queue[Q].empty.isEmpty shouldBe true

  def testSupportFIFO[Q[_]: Queue] =
    check(Prop.forAll { list: List[Int] =>
      val queue = list.foldLeft(Queue[Q].empty[Int]) {
        case (q, i) =>
          q.snoc(i)
      }
      Queue.getAllItems(queue) === list
    })

  "queue" when {
    "empty" should {
      "return true for empty BatchedQueue" in {
        testIsEmpty[BatchedQueue]
      }
    }
    "not empty" should {
      "support FIFO invariant for BatchedQueue" in {
        testSupportFIFO[BatchedQueue]
      }
    }
  }
}
