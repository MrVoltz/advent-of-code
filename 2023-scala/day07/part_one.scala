package day07

import scala.io.Source
import scala.math.Ordering.Implicits._

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Hand(cards: IndexedSeq[Int], bid: Int) {
    val cardCounts: Map[Int, Int] = cards.groupBy(n => n).view.mapValues(_.size).toMap

    val figure: Int =
      if (cardCounts.size == 1) 0 // five of a kind
      else if (cardCounts.values.exists(_ == 4)) 1 // four of a kind
      else if (cardCounts.values.size == 2 && cardCounts.values.exists(_ == 3)) 2 // full house
      else if (cardCounts.values.exists(_ == 3)) 3 // three of a kind
      else if (cardCounts.values.count(_ == 2) == 2) 4 // two pair
      else if (cardCounts.values.count(_ == 2) == 1) 5 // one pair
      else 6 // high card
  }

  val sortedCards = "AKQJT98765432"
  val handPattern = """([A-Z0-9]+)\s+([0-9]+)""".r
  val hands = input.getLines().collect({
    case handPattern(cardsStr, bidStr) => Hand(
      cardsStr.map(sortedCards.indexOf(_)),
      bidStr.toInt)
  }).toSeq

  val handOrdering = Ordering[(Int, Seq[Int])].reverse.on((hand: Hand) => (hand.figure, hand.cards))

  println(hands.sorted(handOrdering).zipWithIndex.map({
    case (Hand(_, bid), rank0) => bid.toLong * (rank0 + 1)
  }).sum)
}