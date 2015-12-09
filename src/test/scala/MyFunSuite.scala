import org.scalatest.FunSuite
import MapUtils.merge


trait Monoid[M] {
  def add(x: M, y: M): M
  def zero: M
}


object Monoid {
  implicit def seqMonoid[T] = new Monoid[Seq[T]] {
    def add(x: Seq[T], y: Seq[T]) = x ++ y
    def zero = Seq.empty
  }

  implicit def intMonoid = new Monoid[Int] {
    def add(x: Int, y: Int) = x + y
    def zero = 0
  }
}


object MapUtils {
  def merge[A, B, M : Monoid](maps: Seq[Map[A, M]]): Map[A, M] = {
    val add = implicitly[Monoid[M]].add _
    maps.reduceLeft { (memo, elem) =>
      elem.foldLeft(memo) { case (memo, (key, value)) =>
        memo.updated(key, memo.get(key).map(add(_, value)).getOrElse(value))
      }
    }
  }
}


class MyFunSuite extends FunSuite {
  test("merges Seq values") {
    val input = Seq(Map(1 -> Seq(true), 2 -> Seq(false)), Map(1 -> Seq(false)))
    val expected = Map(1 -> Seq(true, false), 2 -> Seq(false))
    assert(merge(input) === expected)
  }

  test("merges Int values") {
    val input = Seq(Map('a -> 2), Map('a -> 4, 'b -> 1))
    assert(merge(input) === Map('a -> 6, 'b -> 1))
  }
}
