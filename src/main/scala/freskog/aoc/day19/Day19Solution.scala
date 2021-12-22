package freskog.aoc.day19

import zio._

import freskog.aoc.utils._

object Day19Solution extends ZIOAppDefault {

  def scannerPos(first:LazyList[Int], second:LazyList[Int], matches:Int):Int = {
    val proposedX = for {
      x1 <- first
      x2 <- second
    } yield x1 - x2
    proposedX.dropWhile(
      x => ???
    )
    ???
  }


  def part1(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .runFold(???)(???)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day19/day19-test-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer"))
}
