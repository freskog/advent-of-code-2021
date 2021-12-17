package freskog.aoc.day17

import zio._
import freskog.aoc.utils._

import scala.annotation.tailrec

object Day17Solution extends ZIOAppDefault {

  case class Trench(xL:Int, xR:Int, yT:Int, yB:Int)  {

    val smallestHorVelocity:Int =
      Iterator.from(0).dropWhile(velocity => pos(velocity) < xL).next()

    val largestVerVelocity:Int =
      math.abs(yB+1)

    val validHorVelocities =
      Iterator.from(1 to xR).toList

    val potentialVerVelocities =
      Iterator.from(yB to largestVerVelocity).toList

    def pos(velocity:Int): Int = (velocity*(1 + velocity)) / 2
    def computeTopHeight:Int = pos(largestVerVelocity)

    def allInitialVelocities =
      for {
        vx <- validHorVelocities
        vy <- potentialVerVelocities if valid(vx, vy, 0,0)
      } yield  {
        (vx,vy)
      }

    @tailrec
    private def valid(dx:Int,dy:Int,x:Int,y:Int):Boolean =
      if(hitsBox(x,y)) true else if(missedBox(x,y)) false
      else valid(if(dx > 0) dx-1 else dx, dy - 1,x+dx,y+dy)

    def hitsBox(x:Int,y:Int):Boolean =
      xL <= x && x <= xR && yB <= y && y <= yT

    def missedBox(x:Int,y:Int):Boolean =
      x > xR || y < yB
  }

  val inputPattern = """target area: x=(\d+)\.\.(\d+), y=-(\d+)\.\.-(\d+)""".r

  def part1(inputPath:String) =
    readAllAsString(inputPath).map {
      case inputPattern(xl, xr, yt, yb) => Trench(xl.toInt, xr.toInt, -yb.toInt, -yt.toInt)
    }
      .map(_.computeTopHeight)

  def part2(inputPath:String) =
    readAllAsString(inputPath).map {
      case inputPattern(xl, xr, yt, yb) =>

        Trench(xl.toInt, xr.toInt, -yb.toInt, -yt.toInt)
    }
      .map(_.allInitialVelocities.length)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day17/day17-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day17/day17-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
