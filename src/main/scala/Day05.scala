class Line(lineStr: String):

  private val firstSplit = lineStr.split("->")
  private val fromArray = firstSplit(0).split(",").map(x => x.trim.toInt)
  private val toArray = firstSplit(1).split(",").map(x => x.trim.toInt)

  val from: (Int, Int) = (fromArray(0), fromArray(1))
  val to: (Int, Int) = (toArray(0), toArray(1))

  def addPoints(map: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
    val (startX, stepsX) = (to._1, from._1 - to._1)
    val (startY, stepsY) = (to._2, from._2 - to._2)
    // val listOfPoints = if (stepsX > 0) && (stepsY > 0) then
    //  Seq.empty[(Int, Int)]
    // else
    val listOfPoints = (0 to math.max(math.abs(stepsX), math.abs(stepsY))).map(step => {
      val newX = startX + ((if stepsX != 0 then stepsX / math.abs(stepsX) else 0) * step)
      val newY = startY + ((if stepsY != 0 then stepsY / math.abs(stepsY) else 0) * step)
      (newX, newY)
    })
    listOfPoints.foldLeft(map) { (acc, point) =>
      acc + (point -> (acc.getOrElse(point, 0) + 1))
    }

end Line

@main def day05(): Unit =
  val input = Utils.readInput("day05-1-real.txt").map(line => new Line(line))
  val mapOfPoints = input.foldLeft(Map.empty[(Int, Int), Int]) { (acc, line) =>
    line.addPoints(acc)
  }
  print(mapOfPoints.values.count(i => i >= 2))
