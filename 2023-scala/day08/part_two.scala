package day08

import scala.annotation.tailrec
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val nodePattern = """([A-Z0-9]{3}) = \(([A-Z0-9]{3}), ([A-Z0-9]{3})\)""".r
  val turnsStr :: _ :: nodesStr = input.getLines.toList: @unchecked
  val nodes = nodesStr.map({ case nodePattern(id, leftId, rightId) => id -> (id, leftId, rightId) }).toMap

  @tailrec
  def move(id: String, turnIndex: Int = 0): Int =
    if (id.last == 'Z') turnIndex
    else (turnsStr(turnIndex % turnsStr.length), nodes(id)) match {
      case ('L', (_, nextId, _)) => move(nextId, turnIndex + 1)
      case ('R', (_, _, nextId)) => move(nextId, turnIndex + 1)
    }

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(list: Iterable[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

  println(lcm(nodes.keys.filter(_.last == 'A').map(move(_))))
}