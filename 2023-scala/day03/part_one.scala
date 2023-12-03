import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val lines = input.getLines().toIndexedSeq
  val height = lines.size
  val width = lines.head.length

  def charAt(y: Int, x: Int) =
    if (y >= 0 && y < height && x >= 0 && x < width) lines(y).charAt(x) else '.'

  def isSymbol(y: Int, x: Int) = charAt(y, x) match {
    case c if c.isDigit => false
    case '.' => false
    case _ => true
  }

  val dirs = for (dy <- -1 to 1; dx <- -1 to 1 if dy != 0 || dx != 0) yield (dy, dx)

  @tailrec
  def readingNum(y: Int, x: Int, num: Int = 0, valid: Boolean, sum: Int): Int = charAt(y, x) match {
    case c if c.isDigit =>
      readingNum(y, x + 1, num * 10 + (c - '0'), valid || dirs.exists({ case (dy, dx) => isSymbol(y + dy, x + dx) }),
        sum)

    case _ => skippingUntilNum(y, x + 1, if (valid) sum + num else sum)
  }

  @tailrec
  def skippingUntilNum(y: Int, x: Int, sum: Int): Int = {
    if (y >= height) sum
    else if (x >= width) skippingUntilNum(y + 1, 0, sum)
    else if (charAt(y, x).isDigit) readingNum(y, x, 0, valid = false, sum)
    else skippingUntilNum(y, x + 1, sum)
  }

  println(skippingUntilNum(0, 0, 0))
}
