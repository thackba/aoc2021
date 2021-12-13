import scala.annotation.tailrec
import scala.collection.mutable

type Grid = Array[Array[Int]]

sealed trait Action

case object Increase extends Action

case object Flash extends Action

def buildArray(): Grid =
  Utils.readInput("day11-1-real.txt").map(row => row.map(c => c.toString.toInt).toArray).toArray

def printGrid(grid: Grid): Unit =
  grid.foreach(row => println(row.mkString("")))
  println()

def process(i: Int, j: Int, input: Grid, action: Action, flashSet: mutable.HashSet[(Int, Int)]): Unit =
  action match {
    case Increase =>
      val newLevel = input(i)(j) + 1
      input(i)(j) = newLevel
      if newLevel > 9 && !flashSet.contains((i, j)) then
        process(i, j, input, Flash, flashSet)
    case Flash =>
      flashSet.add((i, j))
      if j > 0 then
        process(i, j - 1, input, Increase, flashSet)
      if j < 9 then
        process(i, j + 1, input, Increase, flashSet)
      if i > 0 then
        process(i - 1, j, input, Increase, flashSet)
        if j > 0 then
          process(i - 1, j - 1, input, Increase, flashSet)
        if j < 9 then
          process(i - 1, j + 1, input, Increase, flashSet)
      if i < 9 then
        process(i + 1, j, input, Increase, flashSet)
        if j > 0 then
          process(i + 1, j - 1, input, Increase, flashSet)
        if j < 9 then
          process(i + 1, j + 1, input, Increase, flashSet)

  }

def countFlashes(input: Grid): Int =
  val flashSet = mutable.HashSet[(Int, Int)]()
  input.indices.foreach { i =>
    input(i).indices.foreach { j =>
      process(i, j, input, Increase, flashSet)
    }
  }
  val counter = flashSet.size
  flashSet.foreach { pos =>
    input(pos._1)(pos._2) = 0
  }
  printGrid(input)
  counter


@main def day11(): Unit =
  val input = buildArray()
  /* Solution 1
  var counter = 0
  (1 to 100).foreach { _ =>
    counter = counter + countFlashes(input)
  } */
  var step = 0
  var flashes = 0
  while flashes != 100 do
    flashes = countFlashes(input)
    step = step + 1
  println(step)
