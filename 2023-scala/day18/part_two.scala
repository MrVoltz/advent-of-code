package day18

import shared.*

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedSet}
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)
  val lines = input.getLines.toList

  val linePattern = """[A-Z] [0-9]+ \(#([0-9a-f]{5})([0-9])\)""".r
  val points = lines.map({
    case linePattern(distHex, dir) => (dir.toInt match {
      case 3 => Dir.north
      case 0 => Dir.east
      case 1 => Dir.south
      case 2 => Dir.west
    }, Integer.parseInt(distHex, 16))
  }).foldLeft(Pos.zero, List.empty[Pos])({ case ((cur, list), (dir, dist)) =>
    val newCur = cur + (dir * dist)
    (newCur, newCur :: list)
  })._2.reverse

  val xMapInv = SortedSet.from(points.flatMap(p => List(p.x, p.x + 1))).toIndexedSeq
  val xMap = xMapInv.zipWithIndex.toMap
  val yMapInv = SortedSet.from(points.flatMap(p => List(p.y, p.y + 1))).toIndexedSeq
  val yMap = yMapInv.zipWithIndex.toMap

  val mappedPoints = points.map(p => Pos(yMap(p.y), xMap(p.x)))

  val boundary = mappedPoints.foldLeft((mappedPoints.last, Set.empty[Pos]))({
    case ((prev, vis), cur) =>
      val dir = prev.dirTo(cur)
      val dist = prev.manhDist(cur)
      (cur, (1 to dist).foldLeft(vis)((vis, dist) => vis + (prev + (dir * dist))))
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
  val area = (for (y <- minPos.y to maxPos.y; x <- minPos.x to maxPos.x; p = Pos(y, x) if !outside(p)) yield {
    val width = xMapInv(p.x + 1) - xMapInv(p.x)
    val height = yMapInv(p.y + 1) - yMapInv(p.y)
    width.toLong * height.toLong
  }).sum
  
  println(area)
}