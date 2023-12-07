package day03

import scala.annotation.tailrec
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  type Pos = (Int, Int)

  val lines = input.getLines().toIndexedSeq
  val height = lines.size
  val width = lines.head.length

  def charAt(y: Int, x: Int) =
    if (y >= 0 && y < height && x >= 0 && x < width) lines(y).charAt(x) else '.'

  def isGear(pos: Pos) = pos match {
    case (y, x) if charAt(y, x) == '*' => true
    case _ => false
  }

  val dirs = for (dy <- -1 to 1; dx <- -1 to 1 if dy != 0 || dx != 0) yield (dy, dx)

  @tailrec
  def readingNum(y: Int, x: Int, num: Int = 0, siblingGears: Set[Pos], gearProds: Map[Pos, (Int, Int)]): Map[Pos, (Int, Int)] =
    charAt(y, x) match {
      case c if c.isDigit =>
        readingNum(y, x + 1, num * 10 + (c - '0'), siblingGears ++ dirs.map({ case (dy, dx) => (y + dy, x + dx) })
          .filter(isGear), gearProds)

      case _ => skippingUntilNum(y, x + 1, siblingGears.foldLeft(gearProds)({ case (map, pos) =>
        val (cnt, prod) = map.getOrElse(pos, (0, 1))
        map.updated(pos, (cnt + 1, prod * num))
      }))
    }

  @tailrec
  def skippingUntilNum(y: Int, x: Int, gearProds: Map[Pos, (Int, Int)]): Map[Pos, (Int, Int)] = {
    if (y >= height) gearProds
    else if (x >= width) skippingUntilNum(y + 1, 0, gearProds)
    else if (charAt(y, x).isDigit) readingNum(y, x, 0, Set.empty, gearProds)
    else skippingUntilNum(y, x + 1, gearProds)
  }

  println(skippingUntilNum(0, 0, Map.empty[Pos, (Int, Int)]).values.foldLeft(0)({ case (sum, (cnt, prod)) =>
    if (cnt == 2) sum + prod else sum
  }))
}
