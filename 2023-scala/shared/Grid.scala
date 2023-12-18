package shared

import java.util.Objects
import scala.annotation.{tailrec, targetName}

case class Pos(y: Int, x: Int) {
  def swap: Pos = Pos(x, y)

  def n: Pos = copy(y = y - 1)

  def s: Pos = copy(y = y + 1)

  def e: Pos = copy(x = x + 1)

  def w: Pos = copy(x = x - 1)

  def nbsPlus: List[Pos] = List(n, s, e, w)

  def nbsDiag: List[Pos] = List(n.e, n.w, s.e, s.w)

  def nbs: List[Pos] = nbsPlus ++ nbsDiag

  def manhDist(p: Pos): Int = (x - p.x).abs + (y - p.y).abs

  def valid(implicit g: Grid): Boolean = g.isValid(this)

  @targetName("add")
  def +(p: Pos): Pos = Pos(y + p.y, x + p.x)

  @targetName("subtract")
  def -(p: Pos): Pos = Pos(y - p.y, x - p.x)

  def dirFrom(p: Pos): Pos = Pos(math.signum(y - p.y), math.signum(x - p.x))

  def dirTo(p: Pos): Pos = Pos(math.signum(p.y - y), math.signum(p.x - x))
}

object Pos {
  val zero: Pos = Pos(0, 0)
}

object Dir {
  val north: Pos = Pos(-1, 0)

  val east: Pos = Pos(0, 1)

  val south: Pos = Pos(1, 0)

  val west: Pos = Pos(0, -1)

  def left(d: Pos): Pos = d match {
    case Dir.north => west
    case Dir.east => north
    case Dir.south => east
    case Dir.west => south
    case _ => throw new IllegalArgumentException()
  }

  def right(d: Pos): Pos = d match {
    case Dir.north => east
    case Dir.east => south
    case Dir.south => west
    case Dir.west => north
    case _ => throw new IllegalArgumentException()
  }
}

class Grid(val lines: IndexedSeq[String]) {
  val w: Int = lines(0).length
  val h: Int = lines.size

  def isValid(p: Pos): Boolean = p.x >= 0 && p.y >= 0 && p.x < w && p.y < h

  def apply(p: Pos): Char = if(isValid(p)) lines(p.y).charAt(p.x) else ' '

  def apply(y: Int, x: Int): Char = apply(Pos(y, x))

  def map(f: Pos => Char): Grid = Grid(h, w, f)

  def transpose: Grid = Grid(w, h, p => lines(p.x).charAt(p.y))

  def rotateRight: Grid = Grid(w, h, p => lines(h - p.x - 1).charAt(p.y))

  def updated(p: Pos, c: Char): Grid = Grid(lines.updated(p.y, lines(p.y).updated(p.x, c)))

  def allPos: IndexedSeq[Pos] = for(y <- 0 until h; x <- 0 until w) yield Pos(y, x)

  def mkString: String = lines.mkString("\n")

  override def equals(obj: Any): Boolean = obj match {
    case g: Grid => lines == g.lines
    case _ => false
  }

  override def hashCode(): Int = Objects.hash(lines)
}

object Grid {
  def apply(lines: Iterable[String]): Grid = new Grid(lines.toIndexedSeq)

  def apply(h: Int, w: Int, f: Pos => Char): Grid = new Grid((0 until h).map(y => (0 until w).map(x => f(Pos(y, x))).mkString))
}

def readGrids(lines: IterableOnce[String]): List[Grid] = {
  @tailrec
  def inner(lines: List[String], curLines: List[String] = Nil, grids: List[Grid] = Nil): List[Grid] = lines match {
    case Nil => Grid(curLines.reverse.toIndexedSeq) :: grids
    case "" :: tail => inner(tail, Nil, Grid(curLines.reverse.toIndexedSeq) :: grids)
    case line :: tail => inner(tail, line :: curLines, grids)
  }

  inner(lines.toList).reverse
}