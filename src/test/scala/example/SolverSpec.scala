import org.scalatest._
import Solver._

class SolverSpec extends FlatSpec with Matchers {
  "Missings" should "lists all numbers when given an empty grid" in {
    (0 to 8).foreach(
      missings(List.fill(81)(0), rows)(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    (0 to 8).foreach(
      missings(List.fill(81)(0), cols)(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    (0 to 8).foreach(
      missings(List.fill(81)(0), squares)(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
  }

  "Missings" should "lists no number when given a solved grid" in {
    val grid = Util.init(
      "435269781" +
      "682571493" +
      "197834562" +
      "826195347" +
      "374682915" +
      "951743628" +
      "519326874" +
      "248957136" +
      "763418259"
    )

    (0 to 8).foreach(
      missings(grid, rows)(_) shouldBe empty
    )
    (0 to 8).foreach(
      missings(grid, cols)(_) shouldBe empty
    )
    (0 to 8).foreach(
      missings(grid, squares)(_) shouldBe empty
    )
  }

  "Missings" should "lists missing numbers when given a typical grid" in {
    val grid = Util.init(
      "  46798  " +
      "26   8   " +
      "    5 4  " +
      "92   5184" +
      " 4 2 1 9 " +
      "1869   25" +
      "  1 9    " +
      "   5   48" +
      "  37246  "
    )

    missings(grid, rows)(0) should contain only (1, 2, 3, 5)
    missings(grid, rows)(1) should contain only (1, 3, 4, 5, 7, 9)
    missings(grid, rows)(2) should contain only (1, 2, 3, 6, 7, 8, 9)

    missings(grid, cols)(0) should contain only (3, 4, 5, 6, 7, 8)
    missings(grid, cols)(1) should contain only (1, 3, 5, 7, 9)
    missings(grid, cols)(2) should contain only (2, 5, 7, 8, 9)

    missings(grid, squares)(0) should contain only (1, 3, 5, 7, 8, 9)
    missings(grid, squares)(1) should contain only (1, 2, 3, 4)
    missings(grid, squares)(2) should contain only (1, 2, 3, 5, 6, 7, 9)

  }

}