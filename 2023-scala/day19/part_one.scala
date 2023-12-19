package day19

import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val lines = input.getLines.toList

  type Part = Map[Char, Int]

  case class Rule(id: String, conds: List[(Char, Boolean, Int, String)], default: String) {
    def eval(part: Part): String =
      conds.find({ case (key, isGt, cmpValue, nextId) =>
        val value = part(key)
        if(isGt) value > cmpValue else value < cmpValue
      }).map(_._4).getOrElse(default)
  }

  val rulePattern = """([a-zA-Z]+)\{([^}]+)}""".r
  val condPattern = """([a-z])([<>])([0-9]+):([a-zA-Z]+)""".r
  val partPattern = """\{([^}]+)}""".r
  val eqPattern = """([a-z])=([0-9]+)""".r

  @tailrec
  def parseRules(lines: List[String], rules: Map[String, Rule] = Map.empty): (Map[String, Rule], List[String]) = lines match {
    case Nil => (rules, Nil)
    case "" :: tail => (rules, tail)
    case rulePattern(id, condsStr) :: tail =>
      val conds :+ defaultCond = condsStr.split(",").toList : @unchecked

      parseRules(tail, rules + (id -> Rule(id, conds.map({
        case condPattern(v, op, num, next) => (v(0), op == ">", num.toInt, next)
      }), defaultCond)))
  }

  @tailrec
  def parseParts(lines: List[String], parts: List[Part] = List.empty): List[Part] = lines match {
    case Nil | "" :: _ => parts.reverse
    case partPattern(mapStr) :: tail =>
      parseParts(tail, mapStr.split(",").map({
        case eqPattern(k, v) => k(0) -> v.toInt
      }).toMap :: parts)
  }

  val (rules, rest) = parseRules(lines)
  val parts = parseParts(rest)

  @tailrec
  def evaluatePart(part: Part, rule: Rule = rules("in")): Boolean = rule.eval(part) match {
    case "A" => true
    case "R" => false
    case nextId => evaluatePart(part, rules(nextId))
  }

  println(lines)
  println(rules)
  println(parts)

  println(parts.filter(evaluatePart(_)).map(_.values.sum).sum)
}