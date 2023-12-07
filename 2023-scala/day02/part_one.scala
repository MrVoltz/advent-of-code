package day02

import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val limits = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  )

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
    case Game(id, turns) if turns.forall(turn => turn.forall({ case (color, num) => limits(color) >= num })) => id
  }).sum[Int].toString)
}
