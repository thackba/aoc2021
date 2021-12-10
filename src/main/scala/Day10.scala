import scala.annotation.tailrec

case class SignCounter(brackets: Seq[Char], corruptedSign: Option[Char]):

  def add(bracket: Char): SignCounter =
    this.copy(brackets = bracket +: brackets)

  def remove(bracket: Char): SignCounter =
    if brackets.head == bracket then
      this.copy(brackets = brackets.tail)
    else
      this.copy(corruptedSign = Some(bracket))

end SignCounter


@tailrec
def scanLine(line: String, acc: SignCounter): SignCounter =
  if line.isEmpty || acc.corruptedSign.nonEmpty then
    return acc
  line.head match {
    case '(' => scanLine(line.tail, acc.add('R'))
    case ')' => scanLine(line.tail, acc.remove('R'))
    case '[' => scanLine(line.tail, acc.add('S'))
    case ']' => scanLine(line.tail, acc.remove('S'))
    case '{' => scanLine(line.tail, acc.add('C'))
    case '}' => scanLine(line.tail, acc.remove('C'))
    case '<' => scanLine(line.tail, acc.add('A'))
    case '>' => scanLine(line.tail, acc.remove('A'))
    case _ => scanLine(line.tail, acc)
  }

@tailrec
def countClosingValue(value: Long, chars: Seq[Char]): Long =
  if chars.isEmpty then
    return value
  chars.head match {
    case 'R' => countClosingValue(value * 5 + 1, chars.tail)
    case 'S' => countClosingValue(value * 5 + 2, chars.tail)
    case 'C' => countClosingValue(value * 5 + 3, chars.tail)
    case 'A' => countClosingValue(value * 5 + 4, chars.tail)
  }

@main def day10(): Unit =
  val input = Utils.readInput("day10-1-real.txt")

  val scanned = input.map(line => scanLine(line, SignCounter(Seq.empty, None)))

  val corruptedScore = scanned.filter(line => line.corruptedSign.nonEmpty).foldLeft(0) { (acc, counter) =>
    counter.corruptedSign match {
      case Some('R') => acc + 3
      case Some('S') => acc + 57
      case Some('C') => acc + 1197
      case Some('A') => acc + 25137
      case _ => acc
    }
  }

  val incompleteScore = scanned.filter(line => line.corruptedSign.isEmpty).map { counter =>
    countClosingValue(0, counter.brackets)
  }.sorted

  println(corruptedScore)
  println(incompleteScore(incompleteScore.size / 2))
