def fuel(n: Int): Int =
  if n == 0 then 0 else n + fuel(n - 1)

@main def day07(): Unit =
  val input = Utils.readInput("day07-1-real.txt").head.split(",").map(s => s.trim.toInt)
  val fuels = (input.min to input.max).map(v => input.map(x => fuel(math.abs(x - v))).sum)
  println(fuels.min)
