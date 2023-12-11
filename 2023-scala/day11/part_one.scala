package day11

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val lines = input.getLines().toIndexedSeq
  val height = lines.size
  val width = lines.head.length

  case class Pos(y: Int, x: Int) extends Ordered[Pos] {
    import scala.math.Ordered.orderingToOrdered

    def dist(that: Pos): Int = (y - that.y).abs + (x - that.x).abs

    def compare(that: Pos): Int = (y, x) compare (that.y, that.x)
  }

  def charAt(y: Int, x: Int) = lines(y).charAt(x)

  val emptyRows = (for(y <- 0 until height if (0 until width).forall(x => charAt(y, x) == '.')) yield y).toSet
  val emptyCols = (for(x <- 0 until width if (0 until height).forall(y => charAt(y, x) == '.')) yield x).toSet

  @tailrec
  def parseField(y: Int, x: Int, seenEmptyRows: Int, seenEmptyCols: Int, galaxies: Set[Pos]): Set[Pos] =
    if(y >= height) galaxies
    else if(x >= width) parseField(y + 1, 0, seenEmptyRows, 0, galaxies)
    else if(emptyRows.contains(y)) parseField(y + 1, 0, seenEmptyRows + 1, 0, galaxies)
    else if(emptyCols.contains(x)) parseField(y, x + 1, seenEmptyRows, seenEmptyCols + 1, galaxies)
    else if(charAt(y, x) == '#') parseField(y, x + 1, seenEmptyRows, seenEmptyCols, galaxies + Pos(y + seenEmptyRows, x + seenEmptyCols))
    else parseField(y, x + 1, seenEmptyRows, seenEmptyCols, galaxies)

  val galaxies = parseField(0, 0, 0, 0, Set.empty)

  println(galaxies)

  println((for(from <- galaxies.iterator; to <- galaxies.iterator if to > from) yield from.dist(to)).sum)
}