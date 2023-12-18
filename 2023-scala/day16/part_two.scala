package day16

import shared.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val grid = readGrids(input.getLines.toList).head

  @tailrec
  def bfsStep(active: Queue[(Pos, Pos)], energ: Set[(Pos, Pos)]): Set[(Pos, Pos)] = active.dequeueOption match {
    case Some(((cur, prev), active2)) =>
      val newPairs = (grid(cur) match {
        case '.' => List(cur + (cur - prev))
        case '/' if prev.n == cur => List(cur.e)
        case '/' if prev.e == cur => List(cur.n)
        case '/' if prev.s == cur => List(cur.w)
        case '/' if prev.w == cur => List(cur.s)
        case '\\' if prev.n == cur => List(cur.w)
        case '\\' if prev.e == cur => List(cur.s)
        case '\\' if prev.s == cur => List(cur.e)
        case '\\' if prev.w == cur => List(cur.n)
        case '|' if prev.n == cur || prev.s == cur => List(cur + (cur - prev))
        case '-' if prev.w == cur || prev.e == cur => List(cur + (cur - prev))
        case '|' => List(cur.n, cur.s)
        case '-' => List(cur.e, cur.w)
      }).filter(p => grid.isValid(p) && !energ.contains((p, cur))).map((_, cur))

      bfsStep(
        newPairs.foldLeft(active2)((active, newPairs) => active.enqueue(newPairs)),
        energ ++ newPairs
      )

    case None => energ
  }
  
  def countEnergized(initial: (Pos, Pos)): Int =
    bfsStep(Queue(initial), Set(initial)).map(_._1).size

  val initial = Iterable(
    (0 until grid.h).map(Pos(_, 0)).map(p => (p, p.e)),
    (0 until grid.h).map(Pos(_, grid.w - 1)).map(p => (p, p.w)),
    (0 until grid.w).map(Pos(0, _)).map(p => (p, p.n)),
    (0 until grid.w).map(Pos(grid.h - 1, _)).map(p => (p, p.s))
  ).flatten

  println(initial.map(countEnergized).max)
}