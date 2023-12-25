package day24

import shared.*

import scala.annotation.{tailrec, targetName}
import scala.io.Source

@main def partOne(inputPath: String): Unit = {
  case class Vec3l(x: Long, y: Long, z: Long) {
    @targetName("add")
    def +(o: Vec3l): Vec3l = Vec3l(x + o.x, y + o.y, z + o.z)
  }

  case class Frac(nom: BigInt, denom: BigInt) {
    @targetName("lte")
    def <=(v: BigInt): Boolean = if (denom < 0) nom >= v * denom else nom <= v * denom

    @targetName("gte")
    def >=(v: BigInt): Boolean = if (denom < 0) nom <= v * denom else nom >= v * denom

    def signum: Int =
      if (nom == 0) 0
      else if (nom.signum == denom.signum) 1
      else -1

    @targetName("add")
    def +(v: BigInt): Frac = Frac(nom + v * denom, denom)

    @targetName("multiply")
    def *(v: BigInt): Frac = Frac(nom * v, denom)
  }

  case class Vec3f(x: Frac, y: Frac, z: Frac)

  case class Vec3d(x: Double, y: Double, z: Double)

  case class Line(p: Vec3l, v: Vec3l) {
    def intersects2d(b: Line): Option[Vec3f] = {
      val as = this.p
      val ad = this.v
      val bs = b.p
      val bd = b.v

      val dx = bs.x - as.x
      val dy = bs.y - as.y
      val det = bd.x * ad.y - bd.y * ad.x

      if (det == 0) None
      else {
        val u = Frac(dy * bd.x - dx * bd.y, det)
        val v = Frac(dy * ad.x - dx * ad.y, det)
        if (u.signum >= 0 && v.signum >= 0) Some(Vec3f(u * ad.x + as.x, u * ad.y + as.y, Frac(0, 1)))
        else None
      }
    }
  }


  val input = Source.fromFile(inputPath)

  val linePattern = """([0-9]+),\s+([0-9\-]+),\s+([0-9\-]+)\s+@\s+([0-9\-]+),\s+([0-9\-]+),\s+([0-9\-]+)""".r
  val stones = input.getLines.map({
    case linePattern(x, y, z, u, v, w) => Line(
      Vec3l(x.toLong, y.toLong, z.toLong),
      Vec3l(u.toLong, v.toLong, w.toLong)
    )
  }).toIndexedSeq

  def insideTestArea(v: Vec3f): Boolean = {
    val min = BigInt("200000000000000")
    val max = BigInt("400000000000000")
    //        val min = 7L
    //        val max = 27L
    v.x >= min && v.y >= min && v.x <= max && v.y <= max
  }

  val pairs = (for (
    i <- stones.indices;
    j <- (i + 1) until stones.size;
    u = stones(i); v = stones(j)
  ) yield (u, v)).toList

  @tailrec
  def calc(pairs: List[(Line, Line)], cnt: Int = 0): Int = pairs match {
    case Nil => cnt
    case (u, v) :: tail =>
      val p = u.intersects2d(v)
      //      println(s"$u, $v, $p")
      p match {
        case Some(p) if insideTestArea(p) =>
          calc(tail, cnt + 1)
        case _ => calc(tail, cnt)
      }
  }

  println(calc(pairs))
}