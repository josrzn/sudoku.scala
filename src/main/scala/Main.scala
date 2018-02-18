import Util._
import Solver._

object Main extends App {
  println("Sudoku")
  val grid = init(
      "       24" +
      "    78  1" +
      "     65  " +
      "      67 " +
      " 5     8 " +
      " 43      " +
      "  23     " +
      "1  45    " +
      "67       "

      // "3  9     " +
      // " 485    9" +
      // " 2  38   " +
      // "87    635" +
      // "1       7" +
      // "532    18" +
      // "   16  4 " +
      // "6    287 " +
      // "     7  6"

      // "53  7    " +
      // "6  195   " +
      // " 98    6 " +
      // "8   6   3" +
      // "4  8 3  1" +
      // "7   2   6" +
      // " 6    28 " +
      // "   419  5" +
      // "    8  79"
      // "  46798  " +
      // "26   8   " +
      // "    5 4  " +
      // "92   5184" +
      // " 4 2 1 9 " +
      // "1869   25" +
      // "  1 9    " +
      // "   5   48" +
      // "  37246  "
  )

  show(grid)
  val solutions = for (g <- solve(grid).distinct if g.forall(0 != _)) yield g
  for (g <- solutions.zipWithIndex){
    println(s"\n\n\nsolution #${g._2}:\n\n")
    show(g._1)
  }
}