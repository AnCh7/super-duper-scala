package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def getElems(heap: H): List[Int] = {
    if (isEmpty(heap)) Nil
    else {
      val min = findMin(heap)
      val newHeap = deleteMin(heap)
      min :: getElems(newHeap)
    }
  }

  property("gen1") = forAll { (h: H) =>
    val min = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(min, h)) == min
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("If you insert any two elements into an empty heap, " +
    "finding the minimum of the resulting heap should get the smallest of the two elements back.") =
    forAll { (a: Int, b: Int) =>
      val heap1 = insert(a, empty)
      val heap2 = insert(b, heap1)
      findMin(heap2) == math.min(a, b)
    }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (a: Int) =>
      val heap1 = insert(a, empty)
      val heap2 = deleteMin(heap1)
      isEmpty(heap2)
    }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") =
    forAll { (h: H) =>
      val elements = getElems(h)
      elements.sorted == elements
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (h1: H, h2: H) =>
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val heap = meld(h1, h2)
      val min3 = findMin(heap)
      (min3 == min1) || (min3 == min2)
    }

  property("Finding a minimum of the melding of a heap with an empty heap should return the minimum of the heap.") =
    forAll { (h: H) =>
      val min = findMin(h)
      val heap1 = meld(h, empty)
      val heap2 = meld(empty, h)
      val min1 = findMin(heap1)
      val min2 = findMin(heap2)
      (min == min1) && (min == min2) && (min1 == min2)
    }

  property("Finding a minimum of the melding of two identical heaps should return a minimum of one or the other.") =
    forAll { (h: H) =>
      val min = findMin(h)
      val heap1 = meld(h, h)
      val heap2 = findMin(heap1)
      min == heap2
    }

  property("Heap sorts") =
    forAll(Gen.choose(0, 100)) { (n: Int) =>

      def insertList(heap: H, list: List[Int]): H = {
        list match {
          case Nil => heap
          case t :: ts => insertList(insert(t, heap), ts)
        }
      }

      val l = (1 to n).toList
      val retrievedElems = getElems(insertList(empty, l))
      retrievedElems == l
    }
}
