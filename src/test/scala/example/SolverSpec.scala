import org.scalatest._
import Solver._

class SolverSpec extends FlatSpec with Matchers {

  "missings" should "lists all numbers when given an empty grid" in {
    (0 to 8).foreach(
      missings(ros)(List.fill(81)(0))(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    (0 to 8).foreach(
      missings(cos)(List.fill(81)(0))(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
    )
    (0 to 8).foreach(
      missings(sqs)(List.fill(81)(0))(_) should contain allOf (1, 2, 3, 4, 5, 6, 7, 8, 9)
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
      missings(ros)(grid)(_) shouldBe empty
    )
    (0 to 8).foreach(
      missings(cos)(grid)(_) shouldBe empty
    )
    (0 to 8).foreach(
      missings(sqs)(grid)(_) shouldBe empty
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

    missings(ros)(grid)(0) should contain only (1, 2, 3, 5)
    missings(ros)(grid)(1) should contain only (1, 3, 4, 5, 7, 9)
    missings(ros)(grid)(2) should contain only (1, 2, 3, 6, 7, 8, 9)

    missings(cos)(grid)(0) should contain only (3, 4, 5, 6, 7, 8)
    missings(cos)(grid)(1) should contain only (1, 3, 5, 7, 9)
    missings(cos)(grid)(2) should contain only (2, 5, 7, 8, 9)

    missings(sqs)(grid)(0) should contain only (1, 3, 5, 7, 8, 9)
    missings(sqs)(grid)(1) should contain only (1, 2, 3, 4)
    missings(sqs)(grid)(2) should contain only (1, 2, 3, 5, 6, 7, 9)

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

    noduplicates(List(ros, cos, sqs))(grid) shouldBe true
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
    )) noduplicates(List(ros, cos, sqs))(g) shouldBe false
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

    possibles(ros, cos, sqs)(missings)(grid)(0) should contain only (3, 5)
    possibles(ros, cos, sqs)(missings)(grid)(1) should contain only (1, 3, 5)
    possibles(ros, cos, sqs)(missings)(grid)(52) shouldBe empty
    possibles(ros, cos, sqs)(missings)(grid)(69) should contain only (2, 3, 7, 9)
    possibles(ros, cos, sqs)(missings)(grid)(80) should contain only (1, 9)
  }

}