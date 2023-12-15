package day15

import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  def hash(s: String): Int =
    s.foldLeft(0)((v, c) => ((v + c.toInt) * 17) % 256)

  def replace(list: List[(String, Int)], label: String, fl: Int): Option[List[(String, Int)]] = list match {
    case (a, b) :: tail if a == label => Some((a, fl) :: tail)
    case head :: tail => replace(tail, label, fl).map(head :: _)
    case Nil => None
  }

  case class AocHashMap(inner: Map[Int, List[(String, Int)]]) {
    def updated(label: String, fl: Int): AocHashMap = {
      val h = hash(label)
      val old = inner.getOrElse(h, List.empty)
      val newList = replace(old, label, fl).getOrElse(old :+ (label, fl))
      AocHashMap(inner + (h -> newList))
    }

    def removed(label: String): AocHashMap = {
      val h = hash(label)
      val old = inner.getOrElse(h, List.empty)
      AocHashMap(inner + (h -> old.filterNot(_._1 == label)))
    }
  }

  val line = input.getLines.next()

  val updatePattern = """^([^=]+)=([0-9+])$""".r
  val removePattern = """^([^-]+)-$""".r

  val map = line.split(",").foldLeft(AocHashMap(Map.empty))((map, insn) => insn match {
    case updatePattern(label, fl) => map.updated(label, fl.toInt)
    case removePattern(label) => map.removed(label)
  })

  println((for((bucket, list) <- map.inner; ((_, fl), slot) <- list.zipWithIndex) yield (bucket + 1) * (slot + 1) * fl).sum)
}