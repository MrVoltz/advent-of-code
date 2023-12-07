package day05

import scala.annotation.tailrec
import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class AocRange(from: Long, len: Long) {
    // inclusive
    def to: Long = from + len - 1

    def valid: Boolean = len > 0

    def toOption: Option[AocRange] = if(valid) Some(this) else None

    def shifted(by: Long): AocRange = AocRange(from + by, len)

    override def toString: String = s"[$from;$to]"
  }

  object AocRange {
    def fromTo(from: Long, to: Long): AocRange = AocRange(from, to - from + 1)
  }

  case class AlmanacMap(id: String, rules: List[(Long, AocRange)] = List.empty) {
    def withRule(rule: (Long, AocRange)): AlmanacMap = copy(rules = rules :+ rule)

    def apply(value: AocRange): List[AocRange] = {
      val (unapplied, applied) = rules.foldLeft((List(value), List.empty[AocRange]))({ case ((unapplied, applied), (dstFrom, ruleSrcRange)) =>
        unapplied.foldLeft((List.empty[AocRange], applied))({ case ((newUnapplied, newApplied), range) =>
          // split into left, center, right

          // part of range before the rule
          val left = AocRange.fromTo(range.from, math.min(range.to, ruleSrcRange.from - 1))
          // part of range inside the rule - needs remapping
          val center = AocRange.fromTo(math.max(range.from, ruleSrcRange.from), math.min(range.to, ruleSrcRange.to))
          // part of range after the rule
          val right = AocRange.fromTo(math.max(range.from, ruleSrcRange.to + 1), range.to)

          (newUnapplied ++ left.toOption ++ right.toOption,
            newApplied ++ center.shifted(dstFrom - ruleSrcRange.from).toOption)
        })
      })
      unapplied ++ applied
    }
  }

  case class Almanac(seedRanges: List[AocRange] = List.empty, maps: List[AlmanacMap] = List.empty) {
    def withMap(map: AlmanacMap): Almanac = copy(maps = maps :+ map)
  }

  val seedsPattern = """^seeds: (.+)$""".r
  val mapHeaderPattern = """^([^ ]+) map:$""".r

  @tailrec
  def parsingAlmanac(lines: List[String], almanac: Almanac = Almanac()): Almanac = lines match {
    case Nil =>
      almanac
    case "" :: rest =>
      parsingAlmanac(rest, almanac)
    case seedsPattern(seedsStr) :: rest =>
      parsingAlmanac(rest, almanac.copy(seedRanges = seedsStr.split(" ").map(_.toLong)
        .grouped(2).map({ case Array(from, len) => AocRange(from, len) }).toList))
    case mapHeaderPattern(mapId) :: rest =>
      parsingAlmanacMap(rest, AlmanacMap(mapId), almanac)
    case _ => throw new IllegalArgumentException()
  }

  @tailrec
  def parsingAlmanacMap(lines: List[String], map: AlmanacMap, almanac: Almanac): Almanac = lines match {
    case Nil | "" :: _ =>
      parsingAlmanac(lines, almanac.withMap(map))
    case line :: rest => line.split(" ").map(_.toLong) match {
      case Array(dstStart, srcStart, len) =>
        parsingAlmanacMap(rest, map.withRule((dstStart, AocRange(srcStart, len))), almanac)
      case _ => throw new IllegalArgumentException()
    }
  }

  val almanac = parsingAlmanac(input.getLines().toList)

  println(almanac.maps.foldLeft(almanac.seedRanges)((seedRanges, map) => seedRanges.flatMap(map.apply)).map(_.from).min)
}