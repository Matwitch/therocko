import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary.arbitrary
import nazva._
import scala.util.Random


class IntegerSuite extends ScalaCheckSuite {

  given [A: Arbitrary]: Arbitrary[Bag[A]] =
    Arbitrary(Gen.listOf(arbitrary[A]).map(Bag(_*)))

  property("Addition and deletion is correct and commutative") {
    forAll { (a: Bag[Int], i: Int) =>
      (Bag.remove(Bag.add(a, i), i) == a) && (
      if a.map.contains(i) then (Bag.add(Bag.remove(a, i), i) == a)
      else true )
    }
  }

  property("Empty bag is always empty") {
    forAll { (a: Int) =>
      Bag.remove(Bag(), a) == Bag() 
    }
  }

  property("Correct toSet conversion") {
    forAll { (s: Set[Int]) =>
      Bag.toSet(Bag(s.toSeq*)) == s
    }
  }

  property("Correct toList conversion") {
    forAll { (s: List[Int]) =>
      Bag.toList(Bag(s.toSeq*)).sorted == s.sorted
    }
  }

  property("Correct toList conversion") {
    forAll { (s: List[Int]) =>
      Bag.toList(Bag(s.toSeq*)).sorted == s.sorted
    }
  }

  property("Correct removing") {
    forAll { (s: Bag[Int]) =>
      var t = s;
      for( a <- 1 to s.size()){
        t = Bag.remove(t, t.map.keySet.head);
      }
      t == Bag();
    }
  }

  property("Correct size") {
    forAll { (s: Seq[Int]) =>
      Bag(s*).size() == s.size 
    }
  }
}