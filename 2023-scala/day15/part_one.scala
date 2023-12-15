package day15

import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  def hash(s: String): Long =
    s.foldLeft(0)((v, c) => ((v + c.toInt) * 17) % 256)

  val line = input.getLines.next()

  println(line.split(",").map(hash).sum)
}