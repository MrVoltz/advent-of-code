package day04

import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Card(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int])

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

  println(input.getLines().flatMap(parseCard).map({ case Card(_, winningNumbers, myNumbers) =>
    val cnt = myNumbers.intersect(winningNumbers).size
    if (cnt == 0) 0 else 1 << (cnt - 1)
  }).sum[Int])
}
