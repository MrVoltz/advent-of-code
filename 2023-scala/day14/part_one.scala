package day14

import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Pos(y: Int, x: Int) {
    def swap: Pos = Pos(x, y)

    def n: Pos = copy(y = y - 1)

    def s: Pos = copy(y = y + 1)

    def e: Pos = copy(x = x + 1)

    def w: Pos = copy(x = x - 1)
  }

  class Grid(val lines: IndexedSeq[String]) {
    val w: Int = lines(0).length
    val h: Int = lines.size

    def isValid(p: Pos): Boolean = p.x >= 0 && p.y >= 0 && p.x < w && p.y < h

    def apply(p: Pos): Char = if(isValid(p)) lines(p.y).charAt(p.x) else ' '

    def apply(y: Int, x: Int): Char = apply(Pos(y, x))

    def map(f: Pos => Char): Grid = Grid(h, w, f)

    def transpose: Grid = Grid(w, h, p => lines(p.x).charAt(p.y))

    def updated(p: Pos, c: Char): Grid = Grid(lines.updated(p.y, lines(p.y).updated(p.x, c)))

    def allPos: IndexedSeq[Pos] = for(y <- 0 until h; x <- 0 until w) yield Pos(y, x)

    def mkString: String = lines.mkString("\n")
  }

  object Grid {
    def apply(lines: Iterable[String]): Grid = new Grid(lines.toIndexedSeq)

    def apply(h: Int, w: Int, f: Pos => Char): Grid = new Grid((0 until h).map(y => (0 until w).map(x => f(Pos(y, x))).mkString))
  }

  @tailrec
  def parseInput(lines: List[String], curLines: List[String] = Nil, grids: List[Grid] = Nil): List[Grid] = lines match {
    case Nil => Grid(curLines.reverse.toIndexedSeq) :: grids
    case "" :: tail => parseInput(tail, Nil, Grid(curLines.reverse.toIndexedSeq) :: grids)
    case line :: tail => parseInput(tail, line :: curLines, grids)
  }

  val grid = parseInput(input.getLines.toList).head

  val newGrid = (0 until grid.h).flatMap(_ => grid.allPos).foldLeft(grid)((grid, pos) => {
    if(grid(pos) == '.' && grid(pos.s) == 'O') grid.updated(pos.s, '.').updated(pos, 'O') else grid
  })

  println(newGrid.mkString)

  println(newGrid.allPos.filter(newGrid(_) == 'O').map(pos => newGrid.h - pos.y).sum)
}