package day04

import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Card(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
    val matchingCnt: Int = myNumbers.intersect(winningNumbers).size
  }

  val cardPattern = """^Card ([^:]+): ([^|]+) \| ([^|]+)$""".r
  val spacePattern = """\s+""".r

  def parseCard(card: String): Option[Card] = card match {
    case cardPattern(idStr, winningStr, myStr) =>
      Some(Card(
        idStr.trim.toInt,
        spacePattern.split(winningStr.trim).map(_.toInt).toSet,
        spacePattern.split(myStr.trim).map(_.toInt).toSet))
    case _ => None
  }

  def updateRange(map: Map[Int, Int], from: Int, to: Int, by: Int): Map[Int, Int] =
    (from to to).foldLeft(map)((map, id) => map.updated(id, map.getOrElse(id, 1) + by))

  println(input.getLines().flatMap(parseCard).foldLeft((Map.empty[Int, Int], 0))({ case ((cardCounts, sum), card) =>
    val thisCardCount = cardCounts.getOrElse(card.id, 1)
    (updateRange(cardCounts, card.id + 1, card.id + card.matchingCnt, thisCardCount), sum + thisCardCount)
  })._2)
}
