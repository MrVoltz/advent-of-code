import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  println(input.getLines().map(line => {
    val digits = line.filter(c => c.isDigit)
    Seq(digits.head, digits.last).mkString.toInt
  }).sum[Int].toString)
}
