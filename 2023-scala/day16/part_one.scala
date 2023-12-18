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

  println(grid.mkString)

  val energized = bfsStep(Queue((Pos.zero, Pos.zero.w)), Set((Pos.zero, Pos.zero.w)))

  val energizedTiles = energized.map(_._1)
  println(grid.map(p => if(energizedTiles.contains(p)) '#' else '.').mkString)

  println(energizedTiles.size)
}