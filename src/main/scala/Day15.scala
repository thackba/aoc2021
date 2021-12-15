val directions = Seq((1, 0), (-1, 0), (0, -1), (0, 1))

case class PointD15(x: Int, y: Int):
  def nextStep(dx: Int, dy: Int): PointD15 = PointD15(x + dx, y + dy)

type GridD15 = Map[PointD15, Int]
extension (grid: GridD15)
  def neighbours(point: PointD15): Seq[PointD15] = directions.map(point.nextStep).filter(grid.contains)

def parse(input: Seq[String]): GridD15 =
  Seq.tabulate(input.head.length, input.size)((x, y) => PointD15(x, y) -> input(y)(x).toInt).flatten.toMap

def path(grid: GridD15): Int =
  val (start, end) = (PointD15(0, 0), grid.keys.maxBy(p => p.x * p.y))
  val risk = collection.mutable.Map(start -> 0)
  val pointsToVisit = collection.mutable.Queue(start)
  while pointsToVisit.nonEmpty do
    val point = pointsToVisit.dequeue()
    grid.neighbours(point).foreach { next =>
      if !risk.contains(next) || risk(point) + grid(next) < risk(next) then
        risk(next) = risk(point) + grid(next)
        pointsToVisit.enqueue(next)
    }
  risk(end)

def expand(grid: GridD15): GridD15 =
  val end = grid.keys.maxBy(p => p.x * p.y)
  val (width, height) = (end.x + 1, end.y + 1)
  List.tabulate(5, 5) { (x, y) =>
    grid.toSeq.map { (point, value) =>
      PointD15(x * width + point.x, y * height + point.y) -> (1 + (value - 1 + x + y) % 9)
    }
  }.flatten.flatten.toMap

@main def day15(): Unit =
  val input = Utils.readInput("day15-1-real.txt")
  println(path(parse(input)))
  println(path(expand(parse(input))))
