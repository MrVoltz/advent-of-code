import scala.io.Source
import scala.math.Ordering.Implicits._

@main def day07PartTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val sortedCards = "AKQT98765432J"
  val J = sortedCards.indexOf('J')

  case class Hand(cards: IndexedSeq[Int], bid: Int) {
    val cardCounts: Map[Int, Int] = cards.groupBy(n => n).view.mapValues(_.size).toMap

    val jCnt: Int = cardCounts.getOrElse(J, 0)

    val figure: Int =
      val cardCountsNoJ = cardCounts.removed(J)
      val pairCnt = cardCountsNoJ.values.count(_ == 2)
      val tripleCnt = cardCountsNoJ.values.count(_ == 3)

      if(jCnt >= 4 || cardCountsNoJ.values.exists(_ >= 5 - jCnt)) 0 // five of a kind
      else if(jCnt >= 3 || cardCountsNoJ.values.exists(_ >= 4 - jCnt)) 1 // four of a kind
      else if((tripleCnt == 1 && (pairCnt == 1 || jCnt >= 1)) || (pairCnt == 2 && jCnt >= 1) || (pairCnt == 1 && jCnt >= 2) || (jCnt >= 3)) 2 // full house
      else if(jCnt >= 2 || cardCountsNoJ.values.exists(_ >= 3 - jCnt)) 3 // three of a kind
      else if((pairCnt == 2) || (pairCnt == 1 && jCnt == 1) || (jCnt >= 2)) 4 // two pair
      else if(jCnt >= 1 || cardCountsNoJ.values.exists(_ >= 2 - jCnt)) 5 // one pair
      else 6 // high card
  }

  val handPattern = """([A-Z0-9]+)\s+([0-9]+)""".r
  val hands = input.getLines().collect({
    case handPattern(cardsStr, bidStr) => Hand(
      cardsStr.map(sortedCards.indexOf(_)),
      bidStr.toInt)
  }).toSeq

  val handOrdering = Ordering[(Int, Seq[Int])].reverse.on((hand: Hand) => (hand.figure, hand.cards))

  println(hands.sorted(handOrdering).zipWithIndex.map({
    case (Hand(_, bid), rank0) => bid.toLong * (rank0 + 1)
  }).sum[Long])
}