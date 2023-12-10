package day10

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val lines = input.getLines().toIndexedSeq
  val height = lines.size
  val width = lines.head.length

  def charAt(pos: Pos) =
    if (pos.valid) lines(pos.y).charAt(pos.x) else '.'

  case class Pos(y: Int, x: Int) {
    def north: Pos = copy(y = y - 1)

    def south: Pos = copy(y = y + 1)

    def east: Pos = copy(x = x + 1)

    def west: Pos = copy(x = x - 1)

    def valid: Boolean = y >= 0 && y < height && x >= 0 && x < width
  }

  def allPos = for (y <- 0 until height; x <- 0 until width) yield Pos(y, x)

  val neighbors: Map[Pos, List[Pos]] = allPos.map(pos => pos -> (charAt(pos) match {
    case '-' => List(pos.east, pos.west)
    case '|' => List(pos.north, pos.south)
    case 'L' => List(pos.north, pos.east)
    case 'J' => List(pos.north, pos.west)
    case 'F' => List(pos.south, pos.east)
    case '7' => List(pos.south, pos.west)
    case _ => List.empty
  })).toMap

  @tailrec
  def bfsStep(active: Queue[Pos], dists: Map[Pos, Int]): Map[Pos, Int] = active.dequeueOption match {
    case None => dists

    case Some((pos, active2)) =>
      val dist = dists(pos)
      val (active3, dists2) = neighbors(pos).filter(nb => !dists.contains(nb))
        .foldLeft((active2, dists))({ case ((newActive, newDists), nb) =>
          (newActive.enqueue(nb), newDists + (nb -> (dist + 1)))
        })
      bfsStep(active3, dists2)
  }

  val startPos = allPos.find(pos => charAt(pos) == 'S').get
  val startPosNbs = List(startPos.north, startPos.east, startPos.south, startPos.west).filter(nb => nb.valid && neighbors(nb).contains(startPos))

  println(startPosNbs.map(nb => {
    val dists = bfsStep(Queue(startPos, nb), Map(startPos -> 0, nb -> 1))
    (startPosNbs.filterNot(_ == nb).map(dists.apply).max + 1) / 2
  }).max)
}