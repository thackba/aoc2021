import scala.io.Source

object Utils:

  def readInput(fileName: String): Seq[String] =
    Source.fromResource(fileName).getLines.toSeq


