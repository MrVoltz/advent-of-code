package day13

import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
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
  }

  @tailrec
  def parseInput(lines: List[String], curLines: List[String] = Nil, grids: List[Grid] = Nil): List[Grid] = lines match {
    case Nil => Grid(curLines.reverse.toIndexedSeq) :: grids
    case "" :: tail => parseInput(tail, Nil, Grid(curLines.reverse.toIndexedSeq) :: grids)
    case line :: tail => parseInput(tail, line :: curLines, grids)
  }

  def findSplit(grid: Grid): Option[Int] =
    (1 until grid.h).find(y => {
      val h = math.min(y, grid.h - y)
      val top = grid.lines.slice(y - h, y)
      val bot = grid.lines.slice(y, y + h)
      bot.reverse == top
    })

  val grids = parseInput(input.getLines.toList).reverse

  println(grids.map(grid => {
    findSplit(grid).map(_ * 100).orElse(findSplit(grid.transpose)).get
  }).sum)
}