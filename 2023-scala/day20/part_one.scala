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
  case class Conj(high: Set[String] = Set.empty) extends Module

  val linePattern = """^([%&]?)([a-z]+) -> ([a-z, ]+)$""".r

  @tailrec
  def parseInput(lines: List[String],
                 modules: Map[String, Module] = Map.empty,
                 in: Map[String, List[String]] = Map.empty,
                 out: Map[String, List[String]] = Map.empty
                ): (Map[String, Module], Map[String, List[String]], Map[String, List[String]]) = lines match {
    case Nil => (modules, in, out)
    case linePattern(kind, id, destsStr) :: tail =>
      val module = kind match {
        case "" => Bcast
        case "%" => Flop()
        case "&" => Conj()
      }
      val dests = destsStr.split(", ")
      parseInput(tail, modules + (id -> module), dests.foldLeft(in)((in, dest) => {
        in + (dest -> in.getOrElse(dest, Nil).appended(id))
      }), out + (id -> dests.toList))
  }

  val (modules, in, out) = parseInput(lines)

  println(modules)
  println(in)
  println(out)

  case class Pulse(src: String, dest: String, high: Boolean)

  def accept(id: String, module: Module, pulse: Pulse): (Module, List[Pulse]) = module match {
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

  @tailrec
  def processPulses(pulses: Queue[Pulse],
                    modules: Map[String, Module],
                    low: Int,
                    high: Int): (Map[String, Module], Int, Int) = pulses.dequeueOption match {
    case None => (modules, low, high)

    case Some((pulse, pulses2)) =>
      println(s"process $pulse")
      modules.get(pulse.dest) match {
        case Some(module) =>
          val (module2, newPulses) = accept(pulse.dest, module, pulse)
          processPulses(
            pulses2 ++ newPulses,
            modules + (pulse.dest -> module2),
            if (pulse.high) low else low + 1,
            if (pulse.high) high + 1 else high
          )

        case None =>
          processPulses(pulses2, modules, if (pulse.high) low else low + 1, if (pulse.high) high + 1 else high)
      }
  }


  val (modules2, low, high) = (0 until 1000).foldLeft((modules, 0, 0))({ case ((modules, low, high), _) =>
    processPulses(Queue(Pulse("", "broadcaster", false)), modules, low, high)
  })

  println(low)
  println(high)

  println(low.toLong * high)
}