def oneDay(fishes: Map[Int, Long]): Map[Int, Long] =
  var newFishes = 0L
  val newMap = fishes.flatMap(entry => {
    if entry._1 > 0 then
      Map((entry._1 - 1) -> entry._2)
    else
      newFishes = entry._2
      Map.empty[Int, Long]
  })
  newMap + (6 -> (newMap.getOrElse(6, 0L) + newFishes)) + (8 -> newFishes)

def show(fishes: Map[Int, Long]): Unit =
  println(fishes.mkString("(", ", ", ")"))

@main def day06(): Unit =
  val input = Utils.readInput("day06-1-real.txt")
    .head.split(",")
    .map(s => s.toInt)
    .foldLeft(Map.empty[Int, Long]) { (acc, element) =>
      acc + (element -> (acc.getOrElse(element, 0L) + 1))
    }

  var current = input
  show(current)

  (1 to 256).foreach(_ => current = oneDay(current))

  show(current)
  val amount = current.values.sum
  println(amount)

