import scala.io.Source

@main def day01(): Unit =
  val input = Utils.readInput("day01-1-real.txt")
  val intSeq = input.map(x => x.toInt)

  // ### Solution 1
  // val data = intSeq
  // ### Solution 2
  val data = intSeq.sliding(3).map(l => l.sum).toSeq

  val zipped = data.zip(Int.MaxValue +: data).count(x => x._1 > x._2)

  println(zipped)
