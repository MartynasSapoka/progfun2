package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    for {
      value <- arbitrary[A]
      next <- oneOf(const(empty), genHeap)
    } yield insert(value, next)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back") =
    forAll { (a: A, b: A) =>
      (a < b) ==> {
        val heap = insert(b, insert(a, empty))
        val min = findMin(heap)
        min == a
      }
    }

  property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") =
    forAll { (a: Int) =>
      val heap = insert(a, empty)
      val rest = deleteMin(heap)
      isEmpty(rest)
    }

  def removeAll(heap: H): List[A] = {
    if (isEmpty(heap)) Nil
    else {
      val min = findMin(heap)
      min :: removeAll(deleteMin(heap))
    }
  }

  def isSorted(s: Seq[A]): Boolean = s match {
    case Seq() => true
    case Seq(_) => true
    case _ => s.sliding(2).forall { case Seq(x, y) => x <= y }
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima") =
    forAll { (h: H) =>
      isSorted(removeAll(h))
    }

  property("Given any two heaps that are melded, you should get a sorted sequence of elements when continually finding and deleting minima") =
    forAll { (h1: H, h2: H) =>
      isSorted(removeAll(meld(h1, h2)))
    }

  property("Insert and remove elements multiple times") = {
    val h1 = insert(5, empty)
    val h2 = insert(4, h1)
    val h3 = deleteMin(h2)
    val h4 = insert(3, h3)
    val h5 = insert(6, h4)
    val h6 = deleteMin(h5)
    val minValue = findMin(h6)
    minValue == 5
  }

  def min(h: H): Option[Int] = if (isEmpty(h)) None else Some(findMin(h))

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll { (h1: H, h2: H) =>
      val heap = meld(h1, h2)
      if (isEmpty(h1) && isEmpty(h2))
        true
      else {
        val minH1 = min(h1)
        val minH2 = min(h2)
        val trueMin = Seq(minH1, minH2).flatten match {
          case xs => xs.min
        }

        val heapMin = findMin(heap)
        trueMin == heapMin
      }
    }
}
