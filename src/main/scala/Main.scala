import Util._
import Solver._

object Main extends App {
  println("Sudoku")
  val grid = init(
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

  show(grid)
  println("rows:")
  println(
    missings(grid, rows).zipWithIndex.
    map{t: Tuple2[List[Int], Int] =>
      t._2 + ": " + t._1.mkString(", ")}.
    mkString("\n")
  )

  println("cols:")
  println(
    missings(grid, cols).zipWithIndex.
    map{t: Tuple2[List[Int], Int] =>
      t._2 + ": " + t._1.mkString(", ")}.
    mkString("\n")
  )

  println("squares:")
  println(
    missings(grid, squares).zipWithIndex.
    map{t: Tuple2[List[Int], Int] =>
      t._2 + ": " + t._1.mkString(", ")}.
    mkString("\n")
  )

  println("index:")
  for ((ns, i) <- possibles(grid, rows, cols, squares).zipWithIndex)
    println(i + ": " + ns.mkString(", "))

}