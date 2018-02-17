import org.scalatest._
import Solver._

class SolverSpec extends FlatSpec with Matchers {

  val ros = (for (i <- 0 to 8) yield {
    (9 * i to 9 * i + 8).toList
  }).toList
  val cos = (for (i <- 0 to 8) yield {
    (i to (9 * 8 + i) by 9).toList
  }).toList
  val sqs = (for (i <- 0 to 2; j <- 0 to 2) yield {
    (for (k <- 0 to 2; l <- 0 to 2) yield {
      i * 27 + j * 3 + k * 9 + l
    }).toList
  }).toList

  "missings" should "lists all numbers when given an empty grid" in {
    (0 to 8).foreach(
      missings(List.fill(81)(0), ros)(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    (0 to 8).foreach(
      missings(List.fill(81)(0), cos)(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    (0 to 8).foreach(
      missings(List.fill(81)(0), sqs)(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
  }

  "missings" should "lists no number for rows, cols or squares when given a solved grid" in {
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
      missings(grid, ros)(_) shouldBe empty
    )
    (0 to 8).foreach(
      missings(grid, cos)(_) shouldBe empty
    )
    (0 to 8).foreach(
      missings(grid, sqs)(_) shouldBe empty
    )
  }

  "missings" should "lists missing numbers when given a typical grid" in {
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

    missings(grid, ros)(0) should contain only (1, 2, 3, 5)
    missings(grid, ros)(1) should contain only (1, 3, 4, 5, 7, 9)
    missings(grid, ros)(2) should contain only (1, 2, 3, 6, 7, 8, 9)

    missings(grid, cos)(0) should contain only (3, 4, 5, 6, 7, 8)
    missings(grid, cos)(1) should contain only (1, 3, 5, 7, 9)
    missings(grid, cos)(2) should contain only (2, 5, 7, 8, 9)

    missings(grid, sqs)(0) should contain only (1, 3, 5, 7, 8, 9)
    missings(grid, sqs)(1) should contain only (1, 2, 3, 4)
    missings(grid, sqs)(2) should contain only (1, 2, 3, 5, 6, 7, 9)

  }

  "noduplicates" should "be fine with a valid grid" in {
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

    noduplicates(grid) shouldBe true
  }

  "noduplicates" should "not like a grid with duplicates" in {
    for (g <- List(
      Util.init(
        "  46798  " +
        "261  8   " +
        "    5 4  " +
        "92   5184" +
        " 4 2 1 9 " +
        "1869   25" +
        "  1 9    " +
        "   5   48" +
        "  37246  "
      ),
      Util.init(
        "  46798  " +
        "26   8   " +
        "    5 4  " +
        "92   5184" +
        " 4 2 1 9 " +
        "1869   25" +
        "  1 9    " +
        "4  5   48" +
        "  37246  "
      ),
      Util.init(
        "  46798  " +
        "26   8   " +
        "    5 4  " +
        "92   5184" +
        " 4 2 1 9 " +
        "1869   25" +
        "  1 9    " +
        "   5 9 48" +
        "  37246  "
      )
    )) noduplicates(g) shouldBe false
  }

  "possibles" should "lists possible numbers when given a typical grid" in {
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

    possibles(grid)(0) should contain only (3, 5)
    possibles(grid)(1) should contain only (1, 3, 5)
    possibles(grid)(52) shouldBe empty
    possibles(grid)(69) should contain only (2, 3, 7, 9)
    possibles(grid)(80) should contain only (1, 9)
  }

}