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

    (for (i <- 0 to 80) yield {
      if (0 == gr(i)) {
        missings(gr, ros)(ro(i)) intersect
        missings(gr, cos)(co(i)) intersect
        missings(gr, sqs)(sq(i))
      } else List()
    }).toList
  }

  def noduplicates(gr: List[Int]) = {
    def nodup(lls: List[List[Int]]) = {
      lls.forall{ls: List[Int] =>
        ls.map(gr(_)).filter(0 != _).size ==
        ls.map(gr(_)).filter(0 != _).distinct.size
      }
    }

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

    nodup(ros) && nodup(cos) && nodup(sqs)
  }

  def explore(grid: List[Int]): (List[Int], List[List[Int]]) = {
    val poss = possibles(grid)
    if (grid.filter(0 == _).size < poss.filter(_.isEmpty).size)
      (List(), poss)
    if (poss.exists(1 == _.size))
      explore(grid.zip(poss).map{
        t: (Int, List[Int]) =>
        t match {
          case (c, _) if c > 0 => c
          case (_, h::Nil) => h
          case _ => 0
        }
      })
    else (grid, poss)
  }

  def solve(grid: List[Int]): List[List[Int]] = {
    val (latents, possibles) = explore(grid)
    if (latents.forall(0 != _))
      List(latents)
    else {
      val firsts = possibles.zipWithIndex.filter(0 < _._1.size)
      if (firsts.isEmpty)
        List(latents)
      else {
        val pos = firsts.head
        (for (po <- pos._1) yield {
          val (l1, l2) = latents.splitAt(pos._2)
          val g = l1 ::: (po :: l2.drop(1))
          if (noduplicates(g))
            solve(g)
          else List(latents)
        }).flatten
      }
    }
  }

}