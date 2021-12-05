package freskog.aoc.day5

import zio._

import freskog.aoc.utils._

object Day5Solution extends ZIOAppDefault {

  case class Point(x:Int, y:Int)
  case class Line(points:Set[Point])
  object Line {
    def parse(str:String, part1: Boolean):Line = {
      val pattern = """(\d+),(\d+) -> (\d+),(\d+)""".r
      val points = str match {
        case pattern(px1,py1,px2,py2) if px1 == px2 =>
          if(py1.toInt < py2.toInt) (py1.toInt to py2.toInt).map(Point(px1.toInt, _))
          else (py2.toInt to py1.toInt).map(Point(px1.toInt, _))
        case  pattern(px1,py1,px2,py2) if py1 == py2 =>
          if(px1.toInt < px2.toInt) (px1.toInt to px2.toInt).map(Point(_, py1.toInt))
          else (px2.toInt to px1.toInt).map(Point(_, py1.toInt))
        case pattern(px1, py1, px2, py2) if part1 => Set.empty
        case pattern(px1, py1, px2, py2) =>
          val xs = if(px1.toInt < (px2.toInt)) (px1.toInt to px2.toInt) else (px2.toInt to px1.toInt).reverse
          val ys = if(py1.toInt < (py2.toInt)) (py1.toInt to py2.toInt) else (py2.toInt to py1.toInt).reverse
          (xs zip ys).map { case (x,y) => Point(x,y)}
      }
      Line(points.toSet)
    }

  }

  def points(inputPath:String, part1:Boolean) =
    readAsOneStringPerLine(inputPath)
      .map(Line.parse(_, part1))
      .runFold(Map.empty[Point, Int].withDefaultValue(0)) {
        case (acc, l) =>
          l.points.foldLeft(acc)(
            (acc2, p) => acc2.updated(p, acc2(p) + 1)
          )
      }.map(
      acc => acc.filter { case ((_, n)) => n > 1 }.size
    )

  def part1(input:String) =
    points(input, true)

  def part2(input:String) =
    points(input, false)


  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day5/day5-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
    part2("day5/day5-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))

}
