import scala.annotation.tailrec
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class AlmanacMap(id: String, rules: List[(Long, Long, Long)] = List.empty) {
    def withRule(rule: (Long, Long, Long)): AlmanacMap = copy(rules = rules :+ rule)

    def apply(value: Long): Long = rules.collectFirst({
      case (dstStart, srcStart, len) if value >= srcStart && value <= srcStart + len =>
        dstStart + (value - srcStart)
    }).getOrElse(value)
  }

  case class Almanac(seeds: List[Long] = List.empty, maps: List[AlmanacMap] = List.empty) {
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
      parsingAlmanac(rest, almanac.copy(seeds = seedsStr.split(" ").map(_.toLong).toList))
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
        parsingAlmanacMap(rest, map.withRule((dstStart, srcStart, len)), almanac)
      case _ => throw new IllegalArgumentException()
    }
  }

  val almanac = parsingAlmanac(input.getLines().toList)

  println(almanac.maps.foldLeft(almanac.seeds)((seeds, map) => seeds.map(map.apply)).min)
}