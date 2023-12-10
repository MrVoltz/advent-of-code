package day10

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
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

    def x3: Pos = Pos(y * 3 + 1, x * 3 + 1)

    def d3: Pos = Pos(y / 3, x / 3)

    def nbs: List[Pos] = List(north, south, east, west)
  }

  def allPos = for (y <- 0 until height; x <- 0 until width) yield Pos(y, x)

  def neighbors(pos: Pos) = charAt(pos) match {
    case '-' => List(pos.east, pos.west)
    case '|' => List(pos.north, pos.south)
    case 'L' => List(pos.north, pos.east)
    case 'J' => List(pos.north, pos.west)
    case 'F' => List(pos.south, pos.east)
    case '7' => List(pos.south, pos.west)
    case _ => List.empty
  }

  def neighborsX3(pos: Pos): List[Pos] = charAt(pos) match {
    case '-' => List(pos.x3.east, pos.x3.west)
    case '|' => List(pos.x3.north, pos.x3.south)
    case 'L' => List(pos.x3.north, pos.x3.east)
    case 'J' => List(pos.x3.north, pos.x3.west)
    case 'F' => List(pos.x3.south, pos.x3.east)
    case '7' => List(pos.x3.south, pos.x3.west)
    case _ => List.empty
  }

  @tailrec
  def bfsStep(active: Queue[Pos], dists: Map[Pos, Int], neighbors: Pos => Iterable[Pos]): Map[Pos, Int] = active.dequeueOption match {
    case None => dists

    case Some((pos, active2)) =>
      val dist = dists(pos)
      val (active3, dists2) = neighbors(pos).filter(nb => !dists.contains(nb))
        .foldLeft((active2, dists))({ case ((newActive, newDists), nb) =>
          (newActive.enqueue(nb), newDists + (nb -> (dist + 1)))
        })
      bfsStep(active3, dists2, neighbors)
  }

  @tailrec
  def reconstructPath(dists: Map[Pos, Int], path: List[Pos]): List[Pos] = {
    val head = path.head
    val headDist = dists(head)
    if (headDist == 0) path
    else reconstructPath(dists, neighbors(head).find(dists(_) == headDist - 1).get :: path)
  }

  val startPos = allPos.find(pos => charAt(pos) == 'S').get
  val startPosNbs = List(startPos.north, startPos.east, startPos.south, startPos.west).filter(nb => nb.valid &&
    neighbors(nb).contains(startPos))

  startPosNbs.foreach(nb => {
    val dists = bfsStep(Queue(startPos, nb), Map(startPos -> 0, nb -> 1), neighbors)
    val endPos = startPosNbs.filterNot(_ == nb).maxBy(dists.apply)
    val path = reconstructPath(dists, List(endPos))
    val pathSet = path.toSet

    // expand maze
    val pathSetX3 = path.flatMap(pos => if (pos == startPos) List(startPos.x3, startPos.x3.north, startPos.x3.east,
      startPos.x3.south, startPos.x3.west) else pos.x3 :: neighborsX3(pos)).toSet

    // flood fill starting at 0,0
    val dists2 = bfsStep(Queue(Pos(0, 0)), Map(Pos(0, 0) -> 0), pos => pos.nbs.filter(nb => nb.d3.valid && !pathSetX3.contains(nb)))

    for (y <- 0 until (height * 3))
      println((for (x <- 0 until width * 3) yield if (pathSetX3.contains(Pos(y, x))) '#' else if(dists2.contains(Pos(y, x))) '@' else '.').mkString)

    // compute area
    println(allPos.count(pos => !pathSet.contains(pos) && !dists2.contains(pos.x3)))
  })
}