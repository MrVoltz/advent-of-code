import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Game(id: Int, turns: List[Map[String, Int]])

  val gamePattern = """^Game ([0-9]+): (.+)$""".r
  val pickPattern = """^([0-9]+) (red|green|blue)$""".r

  def parseGame(game: String): Option[Game] = game match {
    case gamePattern(idStr, turnsStr) =>
      Some(Game(idStr.toInt, turnsStr.split("; ").map(turnStr => turnStr.split(", ").map({
        case pickPattern(num, color) => (color, num.toInt)
      }).toMap).toList))
    case _ => None
  }

  println(input.getLines().flatMap(parseGame).collect({
    case Game(_, turns) =>
      turns.flatten.foldLeft(Map.empty[String, Int])({ case (res, (color, num)) =>
        res.updated(color, res.getOrElse(color, 0).max(num))
      }).values.product
  }).sum[Int].toString)
}
