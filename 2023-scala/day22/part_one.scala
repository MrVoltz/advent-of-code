package day22

import scala.annotation.tailrec
import scala.io.Source

case class Vec3(x: Int, y: Int, z: Int) {
  def down: Vec3 = copy(z = z - 1)
}

case class Brick(a: Vec3, b: Vec3) {
  def down: Brick = Brick(a.down, b.down)

  def isOnGround: Boolean = a.z <= 1 || b.z <= 1

  def intersects(other: Brick): Boolean = {
    def overlap(x1: Int, x2: Int, y1: Int, y2: Int): Boolean =
      x2 >= y1 && x1 <= y2

    overlap(a.x, b.x, other.a.x, other.b.x) &&
      overlap(a.y, b.y, other.a.y, other.b.y) &&
      overlap(a.z, b.z, other.a.z, other.b.z)
  }
}

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val bricks = input.getLines.map(line => {
    line.split("~").map(_.split(",") match {
      case Array(x, y, z) => Vec3(x.toInt, y.toInt, z.toInt)
    }) match {
      case Array(a, b) => Brick(a, b)
    }
  }).toIndexedSeq

  println(bricks)

  def fallStep(bricks: IndexedSeq[Brick]): IndexedSeq[Brick] =
    bricks.indices.foldLeft(bricks)((bricks, i) => {
      val brick = bricks(i)
      val newBrick = brick.down
      val canFall = !brick.isOnGround && bricks.slice(0, i).forall(other => !newBrick.intersects(other)) && bricks
        .drop(i + 1).forall(other => !newBrick.intersects(other))
      if (canFall) bricks.updated(i, newBrick) else bricks
    })

  @tailrec
  def findFixpoint[T](transfer: T => T, state: T): T = {
    val newState = transfer(state)
    if (newState == state) state else findFixpoint(transfer, newState)
  }

  val fallenBricks = findFixpoint(fallStep, bricks)

  println(fallenBricks)

  val disintegrableBricks = fallenBricks.filter(brick => {
    val otherBricks = fallenBricks.filterNot(_ == brick)
    fallStep(otherBricks) == otherBricks
  })

  println(disintegrableBricks.size)
}