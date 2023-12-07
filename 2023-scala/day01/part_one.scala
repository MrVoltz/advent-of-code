package day01

import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  println(input.getLines().map(line => {
    val digits = line.filter(_.isDigit)
    Seq(digits.head, digits.last).mkString.toInt
  }).sum[Int].toString)
}
