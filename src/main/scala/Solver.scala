object Solver {

  def missings(g: List[Int], l: List[List[Int]]): List[List[Int]] = {
    val full = (1 to 9).toList
    for (r <- l) yield (full diff r.map(g(_)))
  }

  def possibles(gr: List[Int]) = {
    def index(lls: List[List[Int]]): Map[Int, Int] =
      lls.zipWithIndex.map{
        t: Tuple2[List[Int], Int] => t._1.map((_, t._2))
      }.flatten.toMap
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
    val ro = index(ros)
    val co = index(cos)
    val sq = index(sqs)

    for (i <- 0 to 80) yield {
      if (0 == gr(i)) {
        missings(gr, ros)(ro(i)) intersect
        missings(gr, cos)(co(i)) intersect
        missings(gr, sqs)(sq(i))
      } else List()
    }
  }

  def reveal(grid: List[Int]): List[Int] = {
    val poss = possibles(grid)
    if (poss.exists(1 == _.size))
      reveal(grid.zip(poss).map{
        t: Tuple2[Int, List[Int]] =>
        t match {
          case (c, _) if c > 0 => c
          case (_, h::Nil) => h
          case _ => 0
        }
      })
    else grid
  }

}