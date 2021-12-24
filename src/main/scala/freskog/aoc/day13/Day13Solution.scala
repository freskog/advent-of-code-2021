package freskog.aoc.day13

import zio._
import freskog.aoc.utils._
import zio.stream._

object Day13Solution extends ZIOAppDefault {

  case class Point(x: Int, y: Int)
  case class Fold(direction: String, axis: Int)

  def decodePoints(input: String): Set[Point] =
    input
      .split("\n")
      .map {
        _.split(",") match { case Array(x, y) => Point(x.toInt, y.toInt) }
      }
      .toSet

  def decodeFolds(input: String): Chunk[Fold] =
    Chunk.fromArray {
      val foldPattern = """fold along (\w)=(\d+)""".r
      input.split("\n").map { case foldPattern(direction, axis) =>
        Fold(direction, axis.toInt)
      }
    }

  def fold(points: Set[Point], fold: Fold): Set[Point] = {
    val (toMove, unchanged) =
      points.partition(p =>
        (fold.direction == "x" && p.x > fold.axis) ||
          (fold.direction == "y" && p.y > fold.axis)
      )
    unchanged ++ toMove.map(p =>
      if (fold.direction == "x") Point(fold.axis - (p.x - fold.axis), p.y)
      else Point(p.x, fold.axis - (p.y - fold.axis))
    )
  }

  def render(points:Set[Point]):String = {
    val maxX = points.maxBy(_.x).x
    val maxY = points.maxBy(_.y).y
    "\n" ++
    (for (y <- 0 to maxY) yield
      (for (x <- 0 to maxX) yield
        if (points.contains(Point(x, y))) "#" else ".").mkString("")
    ).mkString("\n")
  }


  def solve(inputPath: String, headOnly:Boolean) =
    readAsGroupsSplitByEmptyLine(inputPath)
      .peel(ZSink.take[String](1))
      .use { case (pointsChunk, foldStream) =>
        val points = decodePoints(pointsChunk.mkString(""))

        foldStream.runHead
          .someOrFail(new IllegalArgumentException("No folds found"))
          .map(decodeFolds)
          .map(folds =>
            if(headOnly) fold(points, folds.head)
            else folds.foldLeft(points)(fold)
          )
      }

  def part1(inputPath:String) =
    solve(inputPath, true).map(_.size)

  def part2(inputPath:String) =
    solve(inputPath, false).map(render)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day13/day13-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
      part2("day13/day13-input-part-1.txt").flatMap(answer => Console.print(s"Part2: $answer"))
}
