package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(this.empty), genHeap)
  } yield this.insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def getSequence(heap: H): List[Int] = {
    if(isEmpty(heap)) List() else findMin(heap)::getSequence(deleteMin(heap))
  }
  
  property("property heap, not fifo") = forAll { (a: Int, b: Int, c: Int) =>
	  val h1 = insert(a, insert(b, insert(c, empty)))
	  val h2 = insert(c, insert(b, insert(a, empty)))
	  getSequence(h1) == getSequence(h2)
  }
}
