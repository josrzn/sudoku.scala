object Solver {

  val rows = (for (i <- 0 to 8) yield {
    (9 * i to 9 * i + 8).toList
  }).toList
  val cols = (for (i <- 0 to 8) yield {
    (i to (9 * 8 + i) by 9).toList
  }).toList
  val squares = (for (i <- 0 to 2; j <- 0 to 2) yield {
    (for (k <- 0 to 2; l <- 0 to 2) yield {
      i * 27 + j * 3 + k * 9 + l
    }).toList
  }).toList

  def missings(g: List[Int], l: List[List[Int]]): List[List[Int]] = {
    val full = (1 to 9).toList
    for (r <- l) yield (full diff r.map(g(_)))
  }

}