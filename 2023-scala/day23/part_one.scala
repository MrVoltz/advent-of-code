package day23

import shared.*

import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val grid = readGrids(input.getLines).head
  
  def isWalkable(c: Char): Boolean = c != ' ' && c != '#'

  def dfs2(stack: List[(Pos, Set[Pos])], longestDists: Map[Pos, Int]): Map[Pos, Int] = stack match {
    case Nil => longestDists

    case (pos, open) :: tail =>
      val dist = open.size - 1
      val longestDist = longestDists.getOrElse(pos, -1)
      if(longestDist > dist) dfs2(tail, longestDists)
      else {
        val nbs = grid(pos) match {
          case '.' => pos.nbsPlus.filter(nb => isWalkable(grid(nb)))
          case '>' => List(pos.e)
          case '<' => List(pos.w)
          case '^' => List(pos.n)
          case 'v' => List(pos.s)
        }
        val newStack = nbs.filter(!open(_)).map(nb => (nb, open + nb)) ++ tail
        val newLongestDists = longestDists.updated(pos, dist)
        dfs2(newStack, newLongestDists)
      }
  }

  val start = Pos(0, 1)
  val vis = dfs2(List((start, Set(start))), Map.empty)
  
  println(vis(Pos(grid.h - 1, grid.w - 2)))
}