package day08

import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val nodePattern = """([A-Z][A-Z][A-Z]) = \(([A-Z][A-Z][A-Z]), ([A-Z][A-Z][A-Z])\)""".r
  val turnsStr :: _ :: nodesStr = input.getLines.toList: @unchecked
  val nodes = nodesStr.map({ case nodePattern(id, leftId, rightId) => id -> (id, leftId, rightId) }).toMap

  @tailrec
  def move(id: String, turnIndex: Int = 0): Int =
    if (id == "ZZZ") turnIndex
    else (turnsStr(turnIndex % turnsStr.length), nodes(id)) match {
      case ('L', (_, nextId, _)) => move(nextId, turnIndex + 1)
      case ('R', (_, _, nextId)) => move(nextId, turnIndex + 1)
    }

  println(move("AAA"))
}