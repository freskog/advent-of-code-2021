package freskog.aoc.day6

import zio._

import freskog.aoc.utils._

object Day6Solution extends ZIOAppDefault {

  object School {
    def fromFish(fishes:List[Fish]):School =
      fishes.foldLeft(School.empty)(_.add(_,1))

    val empty: School = School(Map.empty)
  }

  case class School(fishes:Map[Fish, Long]) { self =>

    def add(fish:Fish, count:Long):School =
      School(fishes.updated(fish, fishes.getOrElse(fish,0L) + count))

    def generateNewFish(fish:Fish, count:Long):School =
      fish.ageOneDay.foldLeft(self)(_.add(_, count))

    def ageOneDay:School =
      fishes.foldLeft(School.empty) {
          case (school, (fish, count)) => school.generateNewFish(fish, count)
      }

    def count:Long =
      fishes.values.sum
  }

  case class Fish(daysLeft:Long) {
    def ageOneDay:List[Fish] =
      if(daysLeft == 0) List(Fish(6), Fish(8)) else List(Fish(daysLeft - 1))
  }

  def solve(input:String, days:Int) =
    readAsOneStringPerLine(input)
      .runHead
      .someOrFail(new IllegalArgumentException("Empty file"))
      .map(_.split(",").toList.map( n => Fish(n.toLong)))
      .map { initialFish =>
        Iterator
          .iterate(School.fromFish(initialFish))(_.ageOneDay)
          .drop(days)
          .next()
          .count
      }

  def part1(path:String) =
    solve(path, 80)

  def part2(path:String) =
    solve(path, 256)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day6/day6-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
    part2("day6/day6-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))

}
