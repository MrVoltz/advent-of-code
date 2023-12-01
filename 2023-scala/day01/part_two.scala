import scala.annotation.tailrec
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val dict = List(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9',
  )

  println(input.getLines().map(line => {
    @tailrec
    def rec(str: Iterable[Char], acc: List[Char] = Nil): List[Char] = {
      if(str.isEmpty) acc
      else if(str.head.isDigit) rec(str.tail, str.head :: acc)
      else {
        dict.collectFirst {
          case (s, num) if str.take(s.length).mkString == s => num
        } match {
          case Some(value) => rec(str.tail, value :: acc)
          case None => rec(str.tail, acc)
        }
      }
    }
    val digits = rec(line).reverse
    Seq(digits.head, digits.last).mkString.toInt
  }).sum[Int].toString)
}
