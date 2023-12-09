package day09

import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  def extrapolate(seq: List[Long]): Long =
    if(seq.forall(x => x == 0)) 0
    else if(seq.size == 1) seq.last
    else seq.last + extrapolate(seq.sliding(2).map({ case List(a, b) => b - a }).toList)

  println(input.getLines.map(_.split(" ").map(_.toLong).reverse.toList).map(extrapolate).sum)

}