package day06

import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val intPattern = """\d+""".r
  val List(time, distance) = input.getLines().map(intPattern.findAllMatchIn(_).map(_.matched).mkString.toLong).toList
  println((1.toLong to time).map(v => (time - v) * v).count(_ > distance))
}