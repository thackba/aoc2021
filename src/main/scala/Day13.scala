def getDots(input: Seq[String]): Seq[(Int, Int)] =
  input.filter(pred => pred.contains(",")).map(s => s.split(",").map(s => s.trim.toInt)).map(a => (a(1), a(0)))

def getOps(input: Seq[String]): Seq[(String, Int)] =
  input.filter(pred => pred.contains("fold along")).map { s =>
    if s.contains("x") then
      ("x", s.substring(s.indexOf("=") + 1).toInt)
    else
      ("y", s.substring(s.indexOf("=") + 1).toInt)
  }

def fold(input: Seq[(Int, Int)], op: (String, Int)): Seq[(Int, Int)] =
  input.map { pos =>
    if op._1 == "x" then
      (pos._1, if pos._2 > op._2 then pos._2 - (2 * (pos._2 - op._2)) else pos._2)
    else
      (if pos._1 > op._2 then pos._1 - (2 * (pos._1 - op._2)) else pos._1, pos._2)
  }.distinct


def printDots(input: Seq[(Int, Int)]): Unit =
  val maxX = input.map(p => p._1).max
  val maxY = input.map(p => p._2).max
  (0 to maxX).foreach { x =>
    (0 to maxY).foreach { y =>
      if input.contains((x, y)) then
        print("#")
      else
        print(".")
    }
    println()
  }

@main def day13(): Unit =
  val input = Utils.readInput("day13-1-real.txt")
  var dots = getDots(input)
  println(dots)
  val ops = getOps(input)
  println(ops)
  ops.foreach { op =>
    dots = fold(dots, op)

    println(dots)
  }
  printDots(dots)
  println(dots.size)



