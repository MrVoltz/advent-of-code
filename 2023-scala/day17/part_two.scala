package day17

import shared.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val grid = readGrids(input.getLines.toList).head

  type Key = (Pos, Pos, Int)

  case class Entry(cur: Pos, prev: Pos, straight: Int, loss: Int) extends Ordered[Entry] {
    import scala.math.Ordered.orderingToOrdered

    def dir: Pos = cur - prev

    def key: Key = (cur, prev, straight)

    def compare(that: Entry): Int = loss compare that.loss
  }

  // (pos, dir)

  @tailrec
  def bfsStep(q: Queue[Entry], vis: Map[Key, Entry]): Map[Key, Entry] = q.dequeueOption match {
    case Some((e@Entry(cur, prev, straight, loss), q2)) =>
      if(vis.get(e.key).forall(e <= _)) {
        val newEntries = List(
          (cur + Dir.left(e.dir), false),
          (cur + e.dir, true),
          (cur + Dir.right(e.dir), false),
        ).filter(p => grid.isValid(p._1)).collect({
          case (newCur, false) if straight >= 4 => Entry(newCur, cur, 1, loss + (grid(newCur) - '0'))
          case (newCur, true) if straight < 10 => Entry(newCur, cur, straight + 1, loss + (grid(newCur) - '0'))
        }).filter(e => vis.get(e.key).forall(e < _))

        bfsStep(
          newEntries.foldLeft(q2)((q, e) => q.enqueue(e)),
          vis ++ newEntries.map(e => e.key -> e)
        )
      } else bfsStep(q2, vis)

    case None => vis
  }

  val exit = Pos(grid.h - 1, grid.w - 1)

  def compute(initial: Entry): Int = {
    val vis = bfsStep(Queue(initial), Map(initial.key -> initial))
    println("done")
    vis.values.filter(_.cur == exit).map(_.loss).min
  }

  println(Iterable(
    Entry(Pos.zero, Pos.zero.w, 0, 0),
    Entry(Pos.zero, Pos.zero.n, 0, 0)
  ).map(compute).min)

}