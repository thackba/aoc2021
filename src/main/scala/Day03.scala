def sumPosition(l: Seq[IndexedSeq[Int]], p: Int): Int =
  l.foldLeft(0)((acc, subList) => acc + subList(p))

def filterPosition(l: Seq[IndexedSeq[Int]], p: Int, v: Int): Seq[IndexedSeq[Int]] =
  l.filter(subList => subList(p) == v)

def binaryList(amountOfNumbers: Int): Seq[Int] =
  ((amountOfNumbers - 1) to 0 by -1).map(i => math.pow(2, i).toInt)

def readO2CO2(input: Seq[IndexedSeq[Int]], f: Int => Int): Int =
  var current = input
  val amountOfNumbers = input.head.length
  var acc = Seq.empty[Int]
  (0 until amountOfNumbers).foreach { i =>
    val positionSum = sumPosition(current, i)
    val selected = if positionSum.toDouble >= (current.length / 2.0D) then 1 else 0
    val filter = if current.length > 1 then f(selected) else positionSum
    acc = acc :+ filter
    current = filterPosition(current, i, filter)
  }
  binaryList(amountOfNumbers).zip(acc).foldLeft(0) { (acc, zipped) =>
    acc + (zipped._1 * zipped._2)
  }


@main def day03(): Unit =
  val input = Utils.readInput("day03-1-real.txt").map(row => row.map(c => c.toString.toInt))
  val amountOfNumbers = input.head.length

  val added = input.foldLeft(Seq.fill(amountOfNumbers)(0)) { (acc, row) =>
    acc.zip(row).map(zipped => zipped._1 + zipped._2.toString.toInt)
  }
  val (gamma, epsilon) = binaryList(amountOfNumbers).zip(added).foldLeft((0, 0)) { (acc, zipped) =>
    if zipped._2 > input.length / 2 then
      (acc._1 + zipped._1, acc._2)
    else
      (acc._1, acc._2 + zipped._1)
  }

  println(gamma)
  println(epsilon)
  println(gamma * epsilon)

  val o2 = readO2CO2(input, (i: Int) => i)
  val co2 = readO2CO2(input, (i: Int) => Math.abs(i - 1))

  println(o2)
  println(co2)
  println(o2 * co2)
