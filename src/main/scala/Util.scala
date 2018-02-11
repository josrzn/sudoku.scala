object Util {

  def init(g: String) : List[Int] = {
    val g0 = g.replaceAll(" ", "0")
    for (i <- (0 to 80).toList) yield {
      if (i < g0.size && Character.isDigit(g0(i))) g0(i).asDigit
      else 0
    }
  }

  def show(g: List[Int]) = {
    val line = "-----------|-----------|-----------"
    val doubleline = line.replaceAll("-", "=")
    g.zipWithIndex.foreach{ case(e, i) =>
      print(if (0 == e) "   " else s" $e ")
      val j = i + 1
      (j % 81, j % 27, j % 9, j % 3) match {
        case (0, _, _, _) => print("\n")
        case (_, 0, _, _) => print("\n" + doubleline + "\n")
        case (_, _, 0, _) => print("\n" + line + "\n")
        case (_, _, _, 0) => print("|")
        case _ => print(":")
      }
    }
  }

}