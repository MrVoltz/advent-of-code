package day18

import shared.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val linePattern = """([A-Z]) ([0-9]+) \(#[0-9a-f]+\)""".r
  val insns = input.getLines.map({
    case linePattern(dir, dist) => (dir match {
      case "U" => Dir.north
      case "R" => Dir.east
      case "D" => Dir.south
      case "L" => Dir.west
    }, dist.toInt)
  })

  val boundary = insns.foldLeft((Pos.zero, Set.empty[Pos]))({
    case ((cur, vis), (dir, dist)) => (
      cur + (dir * dist),
      (1 to dist).foldLeft(vis)((vis, dist) => vis + (cur + (dir * dist)))
    )
  })._2

  val minPos = Pos(boundary.map(_.y).min, boundary.map(_.x).min) - Pos(1, 1)
  val maxPos = Pos(boundary.map(_.y).max, boundary.map(_.x).max) + Pos(1, 1)

  println(minPos)
  println(maxPos)
  println(boundary.size)

  def isValid(p: Pos): Boolean = p.y >= minPos.y && p.y <= maxPos.y && p.x >= minPos.x && p.x <= maxPos.x

  @tailrec
  def bfsStep(q: Queue[Pos], vis: Set[Pos]): Set[Pos] = q.dequeueOption match {
    case Some(pos, q2) =>
      val newPos = pos.nbsPlus.filter(p => isValid(p) && !boundary(p) && !vis(p))
      bfsStep(q2.enqueueAll(newPos), vis ++ newPos)

    case None => vis
  }

  val outside = bfsStep(Queue(minPos), Set(minPos))
  val area = (for (y <- minPos.y to maxPos.y; x <- minPos.x to maxPos.x; p = Pos(y, x) if !outside(p)) yield 1).sum
  println(area)
}