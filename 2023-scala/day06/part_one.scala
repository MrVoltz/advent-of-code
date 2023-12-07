import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val intPattern = """\d+""".r
  val List(times, distances) = input.getLines().map(intPattern.findAllMatchIn(_).map(_.matched.toInt).toList).toList
  println(times.zip(distances).map({ case (time, distance) =>
    (1 to time).map(v => (time - v) * v).count(_ > distance)
  }).product)
}