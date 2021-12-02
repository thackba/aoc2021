@main def day02(): Unit =
  val input = Utils.readInput("day02-1-real.txt")
  // ### Part 1
  // val result = input.foldLeft((0, 0)) { (acc, line) =>
  //   line.split(" ") match
  //     case Array("forward", value) => (acc._1 + value.toInt, acc._2)
  //     case Array("up", value) => (acc._1, acc._2 - value.toInt)
  //     case Array("down", value) => (acc._1, acc._2 + value.toInt)
  //     case _ => acc
  // }
  // ### Part 2
  val result = input.foldLeft((0, 0, 0)) { (acc, line) =>
    line.split(" ") match
      case Array("forward", value) => (acc._1 + value.toInt, acc._2 + (acc._3 * value.toInt), acc._3)
      case Array("up", value) => (acc._1, acc._2, acc._3 - value.toInt)
      case Array("down", value) => (acc._1, acc._2, acc._3 + value.toInt)
      case _ => acc
  }
  println(result._1 * result._2)
