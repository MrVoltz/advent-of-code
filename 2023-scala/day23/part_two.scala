package day23

import shared.*

import scala.annotation.tailrec
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val grid = readGrids(input.getLines).head

  def isWalkable(c: Char): Boolean = c != ' ' && c != '#'
  
  @tailrec
  def findNext(cur: Pos, prev: Pos, dist: Int): (Pos, List[Pos], Int) = {
    val nbs = cur.nbsPlus.filter(p => p != prev && isWalkable(grid(p)))
    if (nbs.isEmpty || nbs.size > 1) (cur, nbs, dist)
    else findNext(nbs.head, cur, dist + 1)
  }

  case class Graph(nodes: Set[Pos], edges: Map[Pos, Map[Pos, Int]]) {
    def nbs(u: Pos): Map[Pos, Int] = edges.getOrElse(u, Map.empty)

    def addNode(u: Pos): Graph = copy(nodes = nodes + u)

    def addEdge(u: Pos, v: Pos, dist: Int): Graph = copy(edges = edges + (u -> (nbs(u) + (v -> dist))) + (v -> (nbs(v) + (u -> dist))))
  }

  def compress(uNext: Pos, u: Pos, g: Graph): Graph = {
    val (v, vNextList, uvDist) = findNext(uNext, u, 1)
    if(g.nodes(v)) // v is already known
      g.addEdge(u, v, uvDist) // just add edge
    else
      vNextList.foldLeft(g.addNode(v).addEdge(u, v, uvDist))((g, vNext) => compress(vNext, v, g))
  }

  val start = Pos(0, 1)
  val g = compress(start.s, start, Graph(Set(start), Map.empty))

  println(g.edges.mkString("\n"))

  println(grid.map(p => if(g.nodes(p)) '@' else grid(p)).mkString)

  val end = Pos(grid.h - 1, grid.w - 2)

  @tailrec
  def dfs2(stack: List[(Pos, Int, Set[Pos])], longestDist: Int): Unit = stack match {
    case Nil => ()

    case (u, dist, open) :: tail =>
      val nbs = g.edges(u)
      val newStack = nbs.foldLeft(tail)({ case (tail, (v, uvDist)) =>
        if(!open(v)) (v, dist + uvDist, open + v) :: tail
        else tail
      })

      val newLongestDist = if(u == end && dist > longestDist) {
        println(dist)
        dist
      } else longestDist

      dfs2(newStack, newLongestDist)
  }

  dfs2(List((start, 0, Set(start))), -1)
}