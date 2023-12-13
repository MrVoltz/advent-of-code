package day13

import scala.annotation.tailrec
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Pos(y: Int, x: Int) {
    def swap: Pos = Pos(x, y)
  }

  class Grid(val lines: IndexedSeq[String]) {
    val w: Int = lines(0).length
    val h: Int = lines.size

    def isValid(p: Pos): Boolean = p.x >= 0 && p.y >= 0 && p.x < w && p.y < h

    def apply(p: Pos): Char = if(isValid(p)) lines(p.y).charAt(p.x) else ' '

    def apply(y: Int, x: Int): Char = apply(Pos(y, x))

    def transpose: Grid = Grid((0 until w).map(x => (0 until h).map(y => apply(y, x)).mkString))

    def updated(p: Pos, c: Char): Grid = Grid(lines.updated(p.y, lines(p.y).updated(p.x, c)))

    def allPos: IndexedSeq[Pos] = for(y <- 0 until h; x <- 0 until w) yield Pos(y, x)

    def mkString: String = lines.mkString("\n")
  }

  @tailrec
  def parseInput(lines: List[String], curLines: List[String] = Nil, grids: List[Grid] = Nil): List[Grid] = lines match {
    case Nil => Grid(curLines.reverse.toIndexedSeq) :: grids
    case "" :: tail => parseInput(tail, Nil, Grid(curLines.reverse.toIndexedSeq) :: grids)
    case line :: tail => parseInput(tail, line :: curLines, grids)
  }

  def findSplits(grid: Grid): Seq[Int] =
    (1 until grid.h).filter(y => {
      val h = math.min(y, grid.h - y)
      val top = grid.lines.slice(y - h, y)
      val bot = grid.lines.slice(y, y + h)
      bot.reverse == top
    })

  val grids = parseInput(input.getLines.toList).reverse

  println(grids.map(grid => {
    val hSplits = findSplits(grid)
    val vSplits = findSplits(grid.transpose)

    grid.allPos.flatMap(pos => {
      val newGrid = grid.updated(pos, if (grid(pos) == '.') '#' else '.')
      val newHSplits = findSplits(newGrid).diff(hSplits)
      val newVSplits = findSplits(newGrid.transpose).diff(vSplits)
      newHSplits.headOption.map(_ * 100).orElse(newVSplits.headOption)
    }).head
  }).sum)
}