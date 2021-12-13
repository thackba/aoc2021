import scala.collection.mutable.ArrayBuffer

def buildConnections(input: Seq[String]): Map[String, Seq[String]] =
  val pairs = input.map(s => s.split("-").map(t => t.trim))
  pairs.foldLeft(Map.empty[String, Seq[String]]) { (pairMap, row) =>
    pairMap
      + (row(0) -> (pairMap.getOrElse(row(0), Seq.empty[String]) :+ row(1)))
      + (row(1) -> (pairMap.getOrElse(row(1), Seq.empty[String]) :+ row(0)))
  }

def buildPaths(connections: Map[String, Seq[String]], path: Seq[String], result: ArrayBuffer[Seq[String]], usedSmallCaves: Map[String, Int]): Unit =
  if (path.head == "end") then
    result += path
  else
    connections(path.head).foreach { name =>
      if name == "start" then
        ()
      else if name.toLowerCase == name && (usedSmallCaves.getOrElse(name, 0) > 1 || (usedSmallCaves.getOrElse(name, 0) > 0 && usedSmallCaves.values.exists(i => i > 1))) then
        ()
      else if name.toLowerCase == name then
        buildPaths(connections, name +: path, result, usedSmallCaves + (name -> (usedSmallCaves.getOrElse(name, 0) + 1)))
      else
        buildPaths(connections, name +: path, result, usedSmallCaves)
    }


@main def day12(): Unit =
  val input = Utils.readInput("day12-1-real.txt")
  val connections = buildConnections(input)
  println(connections)
  val result = ArrayBuffer.empty[Seq[String]]
  val usedSmallCaves = Map.empty[String, Int]
  buildPaths(connections, Seq("start"), result, usedSmallCaves)
  result.foreach(row => println(row.reverse))
  println(result.size)
