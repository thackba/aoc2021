import scala.annotation.tailrec
import scala.io.Source

def readInput(fileName: String): Seq[String] =
  Source.fromResource(fileName).getLines.toSeq

@tailrec
def applySlidingWindow(rest: Seq[Int], acc: Seq[Int]): Seq[Int] =
  if rest.length < 3 then
    acc
  else
    applySlidingWindow(rest.tail, acc :+ rest.take(3).sum)

@main def day01(): Unit =
  val input = readInput("day01-1-real.txt")
  val intSeq = input.map(x => x.toInt)

  // ### Solution 1
  // val data = intSeq
  // ### Solution 2
  val data = applySlidingWindow(intSeq, Seq.empty[Int])

  val zipped = data.zip(Int.MaxValue +: data).count(x => x._1 > x._2)
  println(zipped)
