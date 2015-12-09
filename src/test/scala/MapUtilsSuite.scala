import org.scalatest.FunSuite
import MapUtils.merge


class MapUtilsSuite extends FunSuite {
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
