package freskog.aoc.day7

import zio._
import freskog.aoc.utils._

object Day7Solution extends ZIOAppDefault {

  def findMiddle(fuel:Vector[Int]) =
    if(fuel.length % 2 == 1) fuel(fuel.length / 2)
    else {
      val leftMiddle = fuel((fuel.length / 2)-1)
      val rightMiddle = fuel(fuel.length / 2)
      val trueMiddle = leftMiddle.toDouble + (rightMiddle - leftMiddle) / 2.0
      if(trueMiddle - leftMiddle < rightMiddle - trueMiddle) leftMiddle else rightMiddle
    }

  def part1(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .runHead
      .someOrFail(new IllegalArgumentException("Empty file"))
      .map( line => line.split(",").map(_.toInt).toVector.sorted)
      .map { fuel =>
        val middle = findMiddle(fuel)
        fuel.map(f => math.abs(f - middle)).sum
      }

  def part2(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .runHead
      .someOrFail(new IllegalArgumentException("Empty file"))
      .map( line => line.split(",").map(_.toInt).toVector.sorted)
      .map { fuel =>
        val costs = (fuel.min to fuel.max).foldLeft(Map.empty[Int,Int]) {
          case (acc, n) => acc.updated(n, acc.getOrElse(n-1, 0) + n)
        }
        val potentialMiddles = (fuel.min to fuel.max).foldLeft(Map.empty[Int,Int]) {
          case (acc, n) => acc.updated(n, fuel.map(n2 => costs(math.abs(n - n2))).sum)
        }
        potentialMiddles.minBy(_._2)._2
      }

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day7/day7-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
    part2("day7/day7-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))
}
