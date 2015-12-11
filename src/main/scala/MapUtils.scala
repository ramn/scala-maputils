trait Monoid[M] {
  def append(x: M, y: M): M
  def zero: M
}


object Monoid {
  implicit def seqMonoid[T] = new Monoid[Seq[T]] {
    def append(x: Seq[T], y: Seq[T]) = x ++ y
    def zero = Seq.empty
  }

  implicit def intMonoid = new Monoid[Int] {
    def append(x: Int, y: Int) = x + y
    def zero = 0
  }
}


object MapUtils {
  def merge[A, B, M : Monoid](maps: Seq[Map[A, M]]): Map[A, M] = {
    val append = implicitly[Monoid[M]].append _
    maps.reduceLeft { (memo, elem) =>
      elem.foldLeft(memo) { case (memo, (key, value)) =>
        memo.updated(key, memo.get(key).map(append(_, value)).getOrElse(value))
      }
    }
  }
}
