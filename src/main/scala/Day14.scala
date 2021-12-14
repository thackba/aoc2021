def readPairs(input: Seq[String]): Map[String, String] =
  input.filter(p => p.contains("->")).map(s => s.split("->")).map(a => (a(0).trim, a(1).trim)).toMap

def step(input: String, pairs: Map[String, String]): String =
  input.head + input.sliding(2).map(s => pairs(s) + s.tail).mkString("")

def simpleSolution(input: String, pairs: Map[String, String]): Unit =
  var s = input
  println(s)
  (1 to 10).foreach { i =>
    s = step(s, pairs)
    println(f"$i%2d > ${s.length}%4d - $s")
  }
  val amounts = s.groupBy(s => s).map(x => (x._1, x._2.length))
  println(amounts)
  val max = amounts.foldLeft(0) { (acc, entry) =>
    if entry._2 > acc then entry._2 else acc
  }
  val min = amounts.foldLeft(Int.MaxValue) { (acc, entry) =>
    if entry._2 < acc then entry._2 else acc
  }
  println(min)
  println(max)
  println(max - min)

def step2(value: Map[String, Long], pairs: Map[String, String]): Map[String, Long] =
  value.foldLeft(Map.empty[String, Long]) { (acc, entry) =>
    val extraChar = pairs(entry._1)
    val one = entry._1(0) + extraChar
    val two = extraChar + entry._1(1)
    acc + (one -> (acc.getOrElse(one, 0L) + entry._2)) + (two -> (acc.getOrElse(two, 0L) + entry._2))
  }

def betterSolution(str: String, pairs: Map[String, String]): Unit =
  var valueMap = str.sliding(2).map(s => (s, 1L)).toMap
  (1 to 40).foreach { i =>
    valueMap = step2(valueMap, pairs)
    println(f"$i%2d > ${valueMap.values.sum}%13d - $valueMap")
  }
  val letterMap = valueMap.foldLeft(Map(str.head.toString -> 1L)) { (acc, entry) =>
    val one = entry._1.tail
    acc + (one -> (acc.getOrElse(one, 0L) + entry._2))
  }
  println(letterMap)
  val max = letterMap.foldLeft(0L) { (acc, entry) =>
    if entry._2 > acc then entry._2 else acc
  }
  val min = letterMap.foldLeft(Long.MaxValue) { (acc, entry) =>
    if entry._2 < acc then entry._2 else acc
  }
  println(min)
  println(max)
  println(max - min)

@main def day14(): Unit =
  val input = Utils.readInput("day14-1-real.txt")
  val pairs = readPairs(input.tail)
  println(pairs)
  simpleSolution(input.head, pairs)
  betterSolution(input.head, pairs)


