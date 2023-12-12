package day12

import scala.io.Source

@main def partTwo(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  println(input.getLines.map(l => {
    val Array(condition, sizesStr) = l.split(" ")
    val sizes = IndexedSeq.from(sizesStr.split(",").map(_.toInt))

    val condition2 = (0 until 5).map(_ => condition).mkString("?")
    val sizes2 = (0 until 5).flatMap(_ => sizes).toIndexedSeq

    val memoKeys = for(i <- 0 to condition2.length; len <- 0 to (sizes2.max + 1); sizeIdx <- 0 to sizes2.size) yield (i, len, sizeIdx)

    val memo = memoKeys.reverseIterator.foldLeft(Map.empty[(Int, Int, Int), Long])({ case (memo, arg) =>
      val (i, len, sizeIdx) = arg

      def damagedCount = memo(i + 1, len + 1, sizeIdx)

      def operationalCount =
        if (len == 0)
          memo(i + 1, 0, sizeIdx)
        else if (sizeIdx < sizes2.size && len == sizes2(sizeIdx))
          memo(i + 1, 0, sizeIdx + 1)
        else 0

      val count: Long = if (len > 0 && (sizeIdx >= sizes2.size || len > sizes2(sizeIdx))) 0
      else if (i == condition2.length) {
        if ((len == 0 && sizeIdx >= sizes2.size) || (len == sizes2(sizeIdx) && sizeIdx == sizes2.size - 1)) 1
        else 0
      } else condition2.charAt(i) match {
        case '#' => damagedCount

        case '.' => operationalCount

        case '?' => damagedCount + operationalCount
      }

      memo + (arg -> count)
    })

//    println(condition2)
//    println(sizes2)

    val count = memo(0, 0, 0)
//    println(count)

    count
  }).sum)
}
