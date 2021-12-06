class Bingo(lines: Seq[String]):

  var numbers: Seq[Seq[Int]] = lines.map(s => s.split(" ").filter(x => x.nonEmpty).map(x => x.trim.toInt))

  def addNumber(number: Int): Unit =
    numbers = numbers.map(se => se.map(v => if v == number then -1 else v))

  def getWinnerValue: Int =
    val hv = numbers.map(se => se.sum)
    val vv = (0 to 4).map(i => numbers.map(se => se(i)).sum)
    if hv.contains(-5) || vv.contains(-5) then
      println(this.numbers)
      numbers.map(se => se.filter(va => va >= 0).sum).sum
    else
      0

end Bingo


@main def day04(): Unit =
  val input = Utils.readInput("day04-1-real.txt")
  val numbers = input.head.split(",").map(x => x.trim.toInt)
  var bingoFields = input.tail.grouped(6).map(lines => new Bingo(lines.tail)).toSeq
  var winnerValues = Seq.empty[Int]
  var numbersLeft = numbers
  // while (winnerValues.count(i => i > 0) == 0) && numbersLeft.nonEmpty do // Solution Part 1
  while numbersLeft.nonEmpty && bingoFields.nonEmpty do
    val nextNumber = numbersLeft.head
    numbersLeft = numbersLeft.tail
    bingoFields.foreach(bingo => bingo.addNumber(nextNumber))
    winnerValues = bingoFields.map(bingo => bingo.getWinnerValue)
    if winnerValues.max > 0 then
      println(nextNumber * winnerValues.max)
    bingoFields = bingoFields.filter(b => b.getWinnerValue == 0)
