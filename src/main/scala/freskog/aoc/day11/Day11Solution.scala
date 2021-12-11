package freskog.aoc.day11

import freskog.aoc.day11.Day11Solution.Dumbos.initialLocations
import zio._
import freskog.aoc.utils._

object Day11Solution extends ZIOAppDefault {

  case class Location(x: Int, y: Int)

  object Dumbos {
    val initialLocations: Set[Location] =
      (0 until 10).flatMap(y => (0 until 10).map(Location(_, y))).toSet

    def from(input:String):Dumbos =
      Dumbos(input.split("\n").map(line => line.toArray.map(_.toString.toInt)))
   }

  case class Dumbos(byRows:Array[Array[Int]]) { self =>

    def neighborsOf(loc: Location): Set[Location] =
     Set(
       Location(loc.x - 1, loc.y - 1), // Left/Up
       Location(loc.x + 1, loc.y + 1), // Right/Down
       Location(loc.x - 1, loc.y + 1), // Left/Down
       Location(loc.x + 1, loc.y - 1), // Right/Up
       Location(loc.x - 1, loc.y),     // Left
       Location(loc.x, loc.y - 1),     // Up
       Location(loc.x + 1, loc.y),     // Right
       Location(loc.x, loc.y + 1)      // Down
     ).filter( l => 0 <= l.x & l.x <= 9 && 0 <= l.y && l.y <= 9)

    def read(loc:Location):Int =
      byRows(loc.y)(loc.x)

    def set(loc:Location, value:Int):Dumbos =
      { byRows(loc.y)(loc.x) = value ; self }

    def nextStep(flashesSoFar:Int): (Dumbos, Int) = {
      def flash(remaining:Chunk[Location], count: Int): Int = {
        if (remaining.isEmpty) count
        else if(read(remaining.head) < 10) {
            set(remaining.head, read(remaining.head) + 1)
            if(read(remaining.head) == 10)
              flash(remaining.tail ++ neighborsOf(remaining.head), count + 1)
            else
              flash(remaining.tail, count)
        } else flash(remaining.tail, count)
      }
      val newCount = flash(Chunk.fromArray(initialLocations.toArray), flashesSoFar)
      initialLocations.filter(read(_) == 10).foreach(set(_, 0))
      (self, newCount)
    }

    def render:String =
      byRows.map(_.mkString("")).mkString("\n","\n","")

    def notAllZeroes:Boolean = byRows.exists(_.exists(_ != 0))
  }

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .runCollect
      .map(_.mkString("\n"))
      .map(Dumbos.from)
      .map { initial =>
        Iterator
          .iterate((initial,0))( p => p._1.nextStep(p._2))
          .drop(100)
          .next()._2
      }

  def part2(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .runCollect
      .map(_.mkString("\n"))
      .map(Dumbos.from)
      .map { initial =>
        Iterator
          .iterate((initial, 0))( p => p._1.nextStep(p._2))
          .takeWhile(_._1.notAllZeroes)
          .toList
          .length

      }

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day11/day11-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
      part2("day11/day11-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))
}
