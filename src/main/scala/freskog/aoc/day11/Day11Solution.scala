package freskog.aoc.day11

import zio._
import freskog.aoc.utils._

object Day11Solution extends ZIOAppDefault {

  case class Location(x: Int, y: Int)

  object Dumbos {
    val initialLocations: Chunk[Location] =
      Chunk.fromIterable((0 until 10).flatMap(y => (0 until 10).map(Location(_, y))))

    def from(input: String): Dumbos =
      Dumbos({
        val array = input.split("\n").map(line => line.toArray.map(_.toString.toInt))
        initialLocations.foldLeft(Map.empty[Location, Int])((acc, l) => acc.updated(l, array(l.y)(l.x)))
      })
  }

  case class Dumbos(byLoc: Map[Location, Int]) { self =>

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
      ).filter(l => 0 <= l.x & l.x <= 9 && 0 <= l.y && l.y <= 9)

    def read(loc: Location): Int =
      byLoc(loc)

    def set(loc: Location, value: Int): Dumbos =
      Dumbos(byLoc.updated(loc, value))

    def nextStep(increment: Chunk[Location]): Dumbos =
      if (increment.isEmpty) Dumbos(byLoc.map { case (l, v) => (l, if (v == 10) 0 else v) })
      else if (byLoc(increment.head) == 10) nextStep(increment.tail)
      else if (byLoc(increment.head) == 9)
        set(increment.head, read(increment.head) + 1).nextStep(increment.tail ++ neighborsOf(increment.head))
      else
        set(increment.head, read(increment.head) + 1).nextStep(increment.tail)

    def flashes: Int =
      byLoc.values.count(_ == 0)

    def notAllZeroes: Boolean =
      flashes != 100

  }

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath).runCollect
      .map(_.mkString("\n"))
      .map(Dumbos.from)
      .map { initial =>
        Iterator
          .iterate(initial)(_.nextStep(Dumbos.initialLocations))
          .take(101)
          .toList
          .map(_.flashes)
          .sum
      }

  def part2(inputPath: String) =
    readAsOneStringPerLine(inputPath).runCollect
      .map(_.mkString("\n"))
      .map(Dumbos.from)
      .map { initial =>
        Iterator
          .iterate(initial)(_.nextStep(Dumbos.initialLocations))
          .takeWhile(_.notAllZeroes)
          .toList
          .length
      }

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day11/day11-test-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day11/day11-test-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
