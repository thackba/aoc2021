extension (s: String)
  def containsAll(a: String): Boolean =
    (0 until a.length).map(i => a.charAt(i)).forall(c => s.contains(c))

@main def day08(): Unit =
  val input = Utils.readInput("day08-1-real.txt")
    .map(s => s.split(" \\| "))
    .map(a => (
      a(0).split(" ").toSeq.map(s => s.trim.sorted),
      a(1).split(" ").toSeq.map(s => s.trim.sorted)
    ))

  val amount = input.foldLeft(0) { (acc, row) =>
    val one = row._1.filter(s => s.length == 2).head
    val seven = row._1.filter(s => s.length == 3).head
    val four = row._1.filter(s => s.length == 4).head
    val eight = row._1.filter(s => s.length == 7).head

    val fourMinusOne = four.filter(c => one.indexOf(c) == -1)

    val fiveLong = row._1.filter(s => s.length == 5)
    val three = fiveLong.find(s => s.containsAll(one)).get
    val five = fiveLong.find(s => s.containsAll(fourMinusOne)).get
    val two = fiveLong.filter(s => s != three && s != five).head

    var sixLong = row._1.filter(s => s.length == 6)
    val nine = sixLong.find(s => s.containsAll(four)).get
    sixLong = sixLong.filter(s => s != nine)
    val six = sixLong.find(s => s.containsAll(fourMinusOne)).get
    val zero = sixLong.filter(s => s != six).head

    val encodeMap = Map(
      zero -> 0, one -> 1, two -> 2, three -> 3, four -> 4, five -> 5, six -> 6, seven -> 7, eight -> 8, nine -> 9
    )

    val value = row._2.map(s => encodeMap(s)).mkString("").toInt

    acc + value

    // # Solution 1
    // acc + row._2.count(s => s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7)
  }
  print(amount)


