object Solver {

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

  def missings(lis: List[List[Int]])(gr: List[Int]): List[List[Int]] = {
    val oneToNine = (1 to 9).toList
    for (is <- lis) yield (oneToNine diff is.map(gr(_)))
  }

  def possibles(rs: List[List[Int]], cs: List[List[Int]], ss: List[List[Int]])
    (mis: List[List[Int]] => List[Int] => List[List[Int]])
    (gr: List[Int]) = {
    def index(lis: List[List[Int]]): Map[Int, Int] =
      lis.zipWithIndex.map{
        t: Tuple2[List[Int], Int] => t._1.map((_, t._2))
      }.flatten.toMap

    val rx = index(rs)
    val cx = index(cs)
    val sx = index(ss)
    
    (for (i <- 0 to 80) yield {
      if (0 == gr(i)) {
          missings(rs)(gr)(rx(i)) intersect
          missings(cs)(gr)(cx(i)) intersect
          missings(ss)(gr)(sx(i))
      } else List()
    }).toList
  }
  def possiblesRCS(gr: List[Int]) = possibles(ros, cos, sqs)(missings)(gr)

  def noduplicates(lllis: List[List[List[Int]]])(gr: List[Int]) = {
    def nodups(llis: List[List[Int]]) = {
      llis.forall{ls: List[Int] =>
        ls.map(gr(_)).filter(0 != _).size ==
        ls.map(gr(_)).filter(0 != _).distinct.size
      }
    }
  
    lllis.forall(nodups)
  }
  def noduplicatesRCS(gr: List[Int]) = noduplicates(List(ros, cos, sqs))(gr)

  def explore(gr: List[Int]): (List[Int], List[List[Int]]) = {
    val poss = possiblesRCS(gr)
    if (gr.filter(0 == _).size < poss.filter(_.isEmpty).size)
      (List(), poss)
    if (poss.exists(1 == _.size))
      explore(gr.zip(poss).map{
        t: (Int, List[Int]) =>
        t match {
          case (c, _) if c > 0 => c
          case (_, h::Nil) => h
          case _ => 0
        }
      })
    else (gr, poss)
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
          if (noduplicatesRCS(g))
            solve(g)
          else List(latents)
        }).flatten
      }
    }
  }

}