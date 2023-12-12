package day12

import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  println(input.getLines.map(l => {
    val Array(condition, sizesStr) = l.split(" ")
    val sizes = List.from(sizesStr.split(",").map(_.toInt))

    def countArrangments(i: Int, len: Int, sizes: List[Int]): Long = {
      def damagedCount = countArrangments(i + 1, len + 1, sizes)

      def operationalCount =
        if (len == 0)
          countArrangments(i + 1, 0, sizes)
        else if (sizes.nonEmpty && len == sizes.head)
          countArrangments(i + 1, 0, sizes.tail)
        else 0

      if (len > 0 && (sizes.isEmpty || len > sizes.head)) 0
      else if (i == condition.length) {
        if ((len == 0 && sizes.isEmpty) || (len == sizes.head && sizes.tail.isEmpty)) 1
        else 0
      } else condition.charAt(i) match {
        case '#' => damagedCount

        case '.' => operationalCount

        case '?' => damagedCount + operationalCount
      }
    }

    val count = countArrangments(0, 0, sizes)
    println(count)

    count
  }).sum)
}