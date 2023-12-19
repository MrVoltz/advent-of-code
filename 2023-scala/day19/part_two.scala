package day19

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)
  val lines = input.getLines.toList

  type Cond = (Char, Boolean, Int, String)

  case class AocRange(values: Map[Char, (Int, Int)]) {
    // (true, false)
    def splitBy(cond: Cond): (Set[AocRange], Set[AocRange]) = {
      val (key, isGt, cmpValue, _) = cond
      val (from, to) = values(key)
      if(isGt) { // [from, to) > cmpValue
        if(from > cmpValue)
          (Set(this), Set.empty)
        else if(to > cmpValue + 1)
          (Set(AocRange(values.updated(key, (cmpValue + 1, to)))), Set(AocRange(values.updated(key, (from, cmpValue + 1)))))
        else
          (Set.empty, Set(this))
      } else { // [from, to) < cmpValue
        if (to <= cmpValue)
          (Set(this), Set.empty)
        else if (from < cmpValue)
          (Set(AocRange(values.updated(key, (from, cmpValue)))), Set(AocRange(values.updated(key, (cmpValue, to)))))
        else
          (Set.empty, Set(this))
      }
    }

    def size: Long = values.values.map({ case (from, to) => (to - from).toLong }).product
  }

  case class Rule(id: String, conds: List[Cond], default: String) {
    def splitRange(range: AocRange): Map[String, Set[AocRange]] = {
      val (falseRanges, map) = conds.foldLeft((Set(range), Map.empty[String, Set[AocRange]]))({ case ((ranges, map), cond) =>
        val splitRanges = ranges.map(_.splitBy(cond))
        val trueRanges = splitRanges.flatMap(_._1)
        val falseRanges = splitRanges.flatMap(_._2)

        (falseRanges, map.updated(cond._4, map.getOrElse(cond._4, Set.empty[AocRange]) ++ trueRanges))
      })
      map.updated(default, map.getOrElse(default, Set.empty[AocRange]) ++ falseRanges)
    }
  }

  val rulePattern = """([a-zA-Z]+)\{([^}]+)}""".r
  val condPattern = """([a-z])([<>])([0-9]+):([a-zA-Z]+)""".r

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

  val (rules, _) = parseRules(lines)

  type State = Map[String, Set[AocRange]]

  def joinStates(first: State, second: State): State =
    (first.keySet ++ second.keySet).map(k => k -> (first.getOrElse(k, Set.empty) ++ second.getOrElse(k, Set.empty))).toMap

  def transfer(state: State): State = {
    state.foldLeft(state)({ case (newState, (ruleId, rangeSet)) =>
      if(ruleId == "A" || ruleId == "R")
        newState
      else {
        val rule = rules(ruleId)
        rangeSet.foldLeft(newState)((newState, range) => joinStates(newState, rule.splitRange(range)))
      }
    })
  }

  @tailrec
  def findFixpoint(state: State): State = {
    val newState = transfer(state)
    if(newState == state)
      state
    else
      findFixpoint(newState)
  }

  val keys = List('x', 'm', 'a', 's')
  val initialRange = AocRange(keys.map(_ -> (1, 4001)).toMap)

  val initialState = Map("in" -> Set(initialRange))
  val fixpoint = findFixpoint(initialState)

  val acceptedRanges = fixpoint("A")
//  println(acceptedRanges)
  println(acceptedRanges.map(_.size).sum)
}