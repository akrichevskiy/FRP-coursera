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

  property("minFromTwo") = forAll { a: Int =>
    val h = insert(a, empty)
    val b = a + a
    val h2 = insert(a, h)
    findMin(h2) == a
  }

  property("min3") = forAll { a: Int =>
    val minA = a / 10
    val h = insert(minA, empty)
    val h1 = insert(minA - 1, h)
    val h2 = insert(minA + 2, h1)
    findMin(h2) == minA - 1
  }

  property("deleteAfterInsert") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2) == true
  }

  property("empty") = forAll { a: Int =>
    val h = empty
    isEmpty(h) == true
  }

  property("meld1") = forAll { a: Int =>
    val b = a - 10
    val h = insert(b + 2, empty)
    val h1 = insert(b, h)

    val f = insert(b + 3, empty)
    val f1 = insert(b + 4, f)

    val r = meld(h1, f1)
    findMin(r) == b
  }

  property("deleteFromMeld") = forAll { a: Int =>
    val b = a - 10
    val h = insert(b, empty)
    val h1 = insert(b + 1, empty)

    val f = insert(b, empty)

    val r = meld(f, h1)

    val r1 = deleteMin(r)
    findMin(r1) == b + 1
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  //
  //  property("gen2") = forAll { (h: H) =>
  ////    val m = if (isEmpty(h)) 0 else findMin(h)
  ////    findMin(insert(m, h)) == m
  //    def go(h:H): H = h match {
  ////      case Nil => List(empty)
  //      case t::ts =>   List(ts)
  //    }
  //    val r = go(h)
  //    isEmpty(r) == true
  //  }

  property("meld2") = forAll { (h: H, h1: H) =>
    val m1 = findMin(h)
    val m2 = findMin(h1)

    val am = if (m1 < m2) m1 else m2

    val r = meld(h, h1)

    findMin(r) == am
  }

  property("sorted-after-minDelete") = forAll { (h: H) =>
    def getMin(t: H, s: List[Int]): (List[Int]) = t match {
      case t if (isEmpty(t)) => s
      case _ =>
        val m = findMin(t)
        val n = deleteMin(t)
        getMin(n, s ++ List(m))
    }


    def isOrdered(l: List[Int]): Boolean = l match {
      case Nil => true
      case x :: Nil => true
      case x :: xs => x <= xs.head && isOrdered(xs)
    }
    val seq = getMin(h, List.empty)

    isOrdered(seq) == true
  }

  property("list") = forAll { (x: List[Int]) =>
    def fill(h: H, l: List[Int]): (H, List[Int]) = l match {
      case Nil => (h, l)
      case t :: ts =>
        val n = insert(t, h)
        fill(n, ts)
    }

    def extract(h1: H, l1: List[Int]): (H, List[Int]) = {
      if (isEmpty(h1)) (empty, l1)
      else {
        val min = findMin(h1)
        val nl = l1 ++ List[Int](min)
        extract(deleteMin(h1), nl)
      }
    }

    val (hh, _) = fill(empty, x)

    if (x.isEmpty) isEmpty(hh) == true
    else findMin(hh) == x.sorted.head


    val (_, ll) = extract(hh, List.empty)
    ll == x.sorted

  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    s <- insert(v, empty)
    //    m <- oneOf(const(empty), genHeap)
    m <- oneOf(const(s), genHeap)
  } yield insert(v, m)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
