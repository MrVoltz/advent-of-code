package day20

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  val input = Source.fromFile(inputPath)

  val lines = input.getLines.toList

  trait Module

  case object Bcast extends Module
  case class Flop(state: Boolean = false) extends Module
  case class Conj(high: Set[Symbol] = Set.empty) extends Module

  val linePattern = """^([%&]?)([a-z]+) -> ([a-z, ]+)$""".r

  @tailrec
  def parseInput(lines: List[String],
                 modules: Map[Symbol, Module] = Map.empty,
                 in: Map[Symbol, List[Symbol]] = Map.empty,
                 out: Map[Symbol, List[Symbol]] = Map.empty
                ): (Map[Symbol, Module], Map[Symbol, List[Symbol]], Map[Symbol, List[Symbol]]) = lines match {
    case Nil => (modules, in, out)
    case linePattern(kind, idStr, destsStr) :: tail =>
      val module = kind match {
        case "" => Bcast
        case "%" => Flop()
        case "&" => Conj()
      }
      val id = Symbol(idStr)
      val dests = destsStr.split(", ").map(Symbol.apply)
      parseInput(tail, modules + (id -> module), dests.foldLeft(in)((in, dest) => {
        in + (dest -> in.getOrElse(dest, Nil).appended(id))
      }), out + (id -> dests.toList))
  }

  val (modules, in, out) = parseInput(lines)

  case class Pulse(src: Symbol, dest: Symbol, high: Boolean)

  def accept(id: Symbol, module: Module, pulse: Pulse): (Module, List[Pulse]) = module match {
    case Bcast => (Bcast, out(id).map(dest => Pulse(id, dest, pulse.high)))
    case Flop(state) =>
      if (pulse.high) (module, Nil)
      else {
        if (state) (Flop(false), out(id).map(dest => Pulse(id, dest, false)))
        else (Flop(true), out(id).map(dest => Pulse(id, dest, true)))
      }
    case Conj(high) =>
      val high2 = if (pulse.high) high + pulse.src else high - pulse.src
      if (high2.size == in(id).size) (Conj(high2), out(id).map(dest => Pulse(id, dest, false)))
      else (Conj(high2), out(id).map(dest => Pulse(id, dest, true)))
  }

  val khIn = in(in(Symbol("rx")).head).toSet
  println(khIn)

  @tailrec
  def processPulses(pulses: Queue[Pulse],
                    modules: Map[Symbol, Module],
                    stats: Map[Symbol, Int]): (Map[Symbol, Module], Map[Symbol, Int]) = pulses
    .dequeueOption match {
    case None => (modules, stats)

    case Some((pulse, pulses2)) =>
      val newStats = if (pulse.high) stats + (pulse.src -> (stats.getOrElse(pulse.src, 0) + 1)) else stats
      modules.get(pulse.dest) match {
        case Some(module) =>
          val (module2, newPulses) = accept(pulse.dest, module, pulse)
          processPulses(
            pulses2 ++ newPulses,
            modules + (pulse.dest -> module2),
            newStats
          )

        case None =>
          processPulses(pulses2, modules, newStats)
      }
  }

  val empty = Symbol("")
  val broadcaster = Symbol("broadcaster")

  @tailrec
  def findPeriods(modules: Map[Symbol, Module], cnt: Int, last: Map[Symbol, Int], periods: Map[Symbol, Int])
  : Map[Symbol, Int] =
    if (khIn.forall(id => periods.contains(id)))
      periods
    else {
      val (newModules, stats) = processPulses(Queue(Pulse(empty, broadcaster, false)), modules, Map.empty)
      val (newLast, newPeriods) = khIn.foldLeft((last, periods))({ case ((last, periods), id) =>
        if (stats.getOrElse(id, 0) > 0)
          (last.get(id), periods.get(id)) match {
            case (_, Some(_)) => (last, periods) // done
            case (Some(lastCnt), _) => (last, periods + (id -> (cnt - lastCnt))) // update periods
            case (None, _) => (last + (id -> cnt), periods) // update last
          }
        else (last, periods)
      })
      findPeriods(newModules, cnt + 1, newLast, newPeriods)
    }

  val periods = findPeriods(modules, 0, Map.empty, Map.empty)
  println(periods)

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(list: Iterable[Long]): Long = list.foldLeft(1.toLong)((a, b) => (a / gcd(a, b)) * b)

  println(lcm(periods.values.map(_.toLong)))
}