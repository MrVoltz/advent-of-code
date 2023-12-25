//> using dep org.scala-lang.modules:scala-parallel-collections_3:1.0.4

package day25

import shared.Pos

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random
import scala.collection.parallel.CollectionConverters._

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  case class Graph(nodes: Map[Symbol, Int], edges: Map[Symbol, Map[Symbol, Int]]) {
    def nbs(u: Symbol): Map[Symbol, Int] = edges.getOrElse(u, Map.empty)

    def addNode(u: Symbol, cnt: Int): Graph = copy(nodes = nodes + (u -> cnt))

    def addEdge(u: Symbol, v: Symbol, cnt: Int): Graph = copy(edges = edges + (u -> (nbs(u) + (v -> cnt))) + (v ->
      (nbs(v) + (u -> cnt))))

    def removeEdge1(u: Symbol, v: Symbol): Graph = copy(edges = edges + (u -> nbs(u).removed(v)))

    def removeNode(u: Symbol): Graph = copy(nodes = nodes.removed(u))

    def condense(u: Symbol, v: Symbol): Graph = {
      val uNbs = nbs(u).filter(nb => nb._1 != u && nodes.contains(nb._1))
      val g2 = copy(nodes = nodes.removed(v).updated(u, nodes(u) + nodes(v)), edges = edges.removed(v).updated(u, uNbs))
      nbs(v).filter(nb => nb._1 != u && nb._1 != v && nodes.contains(nb._1)).foldLeft(g2)({ case (g, (w, cnt)) =>
        g.addEdge(u, w, uNbs.getOrElse(w, 0) + cnt)
      })
    }
  }

  object Graph {
    val empty: Graph = new Graph(Map.empty, Map.empty)
  }

  val g = input.getLines.flatMap(line => {
    val Array(id, nbsStr) = line.split(": ")
    val nbs = nbsStr.split(" ")
    nbs.map(nb => Symbol(id) -> Symbol(nb))
  }).foldLeft(Graph.empty)({ case (g, (u, v)) =>
    g.addNode(u, 1).addNode(v, 1).addEdge(u, v, 1)
  })

  println(g.nodes.mkString("\n"))
  println(g.edges.mkString("\n"))

  @tailrec
  def karger(g: Graph, rng: Random): Graph = if (g.nodes.size <= 2) g else {
    val nodes = g.nodes.keysIterator.toIndexedSeq
    val u = nodes(rng.nextInt(nodes.size))
    val nbs = g.nbs(u).keysIterator.filter(nb => nb != u && g.nodes.contains(nb)).toIndexedSeq
    val v = nbs(rng.nextInt(nbs.size))

    karger(g.condense(u, v), rng)
  }

  val offset = 134218579

  val x = (0 until Int.MaxValue).flatMap(i => {
    val seed = offset + i
    val rng = new Random(seed)
    val g2 = karger(g, rng)
    val List(u, v) = g2.nodes.keys.toList
    val cnt = g2.nbs(u)(v)
//    println(cnt)
    if (cnt == 3) {
      val cnt2 = g2.nbs(v)(u)
      println(s"seed ${seed} nodes ${g2.nodes} cnt ${cnt} cnt2 ${cnt2} product ${g2.nodes.values.product}")
      Some(seed)
    } else None
  })

  println(s"result ${x}")
}