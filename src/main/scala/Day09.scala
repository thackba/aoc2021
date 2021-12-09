import scala.annotation.tailrec

case class Point(i: Int, j: Int)

def lowPoints(input: Seq[Seq[Int]], i: Int, j: Int): Int =
  var lower = 0
  var fields = 0
  val positionValue = input(i)(j)
  if i > 0 then
    fields = fields + 1
    if input(i - 1)(j) > positionValue then lower = lower + 1
  if i < input.length - 1 then
    fields = fields + 1
    if input(i + 1)(j) > positionValue then lower = lower + 1
  if j > 0 then
    fields = fields + 1
    if input(i)(j - 1) > positionValue then lower = lower + 1
  if j < input(i).length - 1 then
    fields = fields + 1
    if input(i)(j + 1) > positionValue then lower = lower + 1
  if lower < fields then -1 else positionValue

@tailrec
def findBasin(input: Seq[Seq[Int]], acc: Set[Point]): Set[Point] =
  var newPoints = Set.empty[Point]
  for (p <- acc)
    if p.i > 0 then
      if input(p.i - 1)(p.j) != 9 then newPoints = newPoints + Point(p.i - 1, p.j)
    if p.i < input.length - 1 then
      if input(p.i + 1)(p.j) != 9 then newPoints = newPoints + Point(p.i + 1, p.j)
    if p.j > 0 then
      if input(p.i)(p.j - 1) != 9 then newPoints = newPoints + Point(p.i, p.j - 1)
    if p.j < input(p.i).length - 1 then
      if input(p.i)(p.j + 1) != 9 then newPoints = newPoints + Point(p.i, p.j + 1)
  val newAcc = acc ++ newPoints
  if newAcc.size != acc.size then
    findBasin(input, newAcc)
  else
    newAcc


@main def day09(): Unit =
  val input = Utils.readInput("day09-1-real.txt").map(s => s.map(c => c.toString.toInt))

  var points = Seq.empty[Point]
  var amount = 0
  for (i <- input.indices) {
    for (j <- input(i).indices) {
      val low = lowPoints(input, i, j)
      if low >= 0 then
        print("X")
        points = points :+ Point(i, j)
      else
        print(input(i)(j))
      amount = amount + 1 + low
    }
    println()
  }

  var basins = Seq.empty[Set[Point]]
  for (p <- points)
    basins = basins :+ findBasin(input, Set(p))

  println(amount)
  println(basins.map(s => s.size).sorted.reverse.take(3).product)
