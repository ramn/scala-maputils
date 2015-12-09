import org.scalatest.FunSuite


trait Monoid[M] {
  def add(x: M, y: M): M
  def zero: M
}


object Monoid {
  implicit def seqMonoid[T] = new Monoid[Seq[T]] {
    def add(x: Seq[T], y: Seq[T]) = x ++ y
    def zero = Seq.empty
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

  def mergeSeqVals[A, B](maps: Seq[Map[A, Seq[B]]]): Map[A, Seq[B]] =
    maps.reduceLeft { (memo, elem) =>
      elem.foldLeft(memo) { case (memo, (key, value)) =>
        memo.updated(key, memo.get(key).map(_ ++ value).getOrElse(value))
      }
    }
}


class MyFunSuite extends FunSuite {
  test("true is true") {
    val x = Seq(Map(1 -> Seq(true), 2 -> Seq(false)), Map(1 -> Seq(false)))
    val expected = Map(1 -> Seq(true, false), 2 -> Seq(false))

    import MapUtils._
    assert(mergeSeqVals(x) === expected)
    assert(merge(x) === expected)
  }
}
