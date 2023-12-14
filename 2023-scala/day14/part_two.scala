package day14

import shared._

import java.util.Objects
import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val grid = readGrids(input.getLines.toList).head

  def doGravity(grid: Grid): Grid = (0 until grid.h).flatMap(_ => grid.allPos).foldLeft(grid)((grid, pos) => {
    if(grid(pos) == '.' && grid(pos.s) == 'O') grid.updated(pos.s, '.').updated(pos, 'O') else grid
  })

  def whirlpool(grid: Grid): Grid = (0 until 4).foldLeft(grid)((grid, _) => doGravity(grid).rotateRight)

  @tailrec
  def findPeriod(grid: Grid, step: Int = 0, seen: Map[Grid, Int] = Map.empty): (Grid, Int, Int) = seen.get(grid) match {
    case Some(seenStep) => (grid, seenStep, step - seenStep)
    case None => findPeriod(whirlpool(grid), step + 1, seen + (grid -> step))
  }

  val (result, lead, period) = findPeriod(grid)
  println(s"$period $lead")

  val numSteps = (1000000000 - lead) % period
  val result2 = (0 until numSteps).foldLeft(result)((grid, _) => whirlpool(grid))

  println(result2.allPos.filter(result2(_) == 'O').map(pos => result2.h - pos.y).sum)
}