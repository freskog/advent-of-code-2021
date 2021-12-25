package freskog.aoc.day20

import freskog.aoc.utils._
import zio._
import zio.stream.ZSink

object Day20Solution extends ZIOAppDefault {

  case class Pos(x: Int, y: Int) {
    def adjacent: List[Pos] =
      List(
        Pos(x - 1, y - 1),
        Pos(x, y - 1),
        Pos(x + 1, y - 1),
        Pos(x - 1, y),
        Pos(x, y),
        Pos(x + 1, y),
        Pos(x - 1, y + 1),
        Pos(x, y + 1),
        Pos(x + 1, y + 1)
      )
  }

  case class Image(algo: String, pixels: Map[Pos, String], width: Int, height: Int, defaultValue:String) {

    def pixelAt(pos: Pos): String =
      if (pos.x < 0 || pos.y < 0 || pos.x >= width || pos.y >= height) defaultValue
      else pixels(pos)

    def nextPixelAt(pos: Pos): String =
      algo.charAt(Integer.parseInt(pos.adjacent.map(pixelAt).mkString, 2)).toString

    def enhance: Image = {
      val nextPixels: Map[Pos, String] =
        (for {
          y <- -2 to height+1
          x <- -2 to width+1
        } yield Pos(x+2, y+2) -> nextPixelAt(Pos(x, y))).toMap
      val newWidth = nextPixels.maxBy(_._1.x)._1.x + 1
      val newHeight = nextPixels.maxBy(_._1.y)._1.y + 1
      val newDefaultValue = if(defaultValue == "0") algo.charAt(0).toString else algo.charAt(511).toString
      Image(algo, nextPixels.withDefaultValue(newDefaultValue), newWidth, newHeight, newDefaultValue)
    }

    override def toString: String =
      "\n" ++ (for (y <- -1 until height+1) yield (for (x <- -1 until width) yield if (pixels(Pos(x, y)) == "1") "#" else ".").mkString).mkString("\n")
  }

  object Image {
    def from(algoRaw: String, image: String) = {
      val algo        = algoRaw.replace('#', '1').replace('.', '0')
      val (_, pixels) = image.foldLeft((Pos(0, 0), Map.empty[Pos, String])) {
        case ((pos, acc), '\n') => (Pos(0, pos.y + 1), acc)
        case ((pos, acc), '#')  => (Pos(pos.x + 1, pos.y), acc.updated(pos, "1"))
        case ((pos, acc), '.')  => (Pos(pos.x + 1, pos.y), acc.updated(pos, "0"))
      }
      val newWidth = pixels.maxBy(_._1.x)._1.x + 1
      val newHeight = pixels.maxBy(_._1.y)._1.y + 1
      val newDefaultValue = "0"
      Image(algo, pixels.withDefaultValue(newDefaultValue), newWidth, newHeight, newDefaultValue)
    }
  }

  def solve(inputPath: String, times:Int) =
    readAsGroupsSplitByEmptyLine(inputPath)
      .peel(ZSink.take[String](1))
      .use { case (algo, imageStream) =>
        imageStream
          .run(ZSink.last)
          .map(image => Image.from(algo.mkString, image.get))
          .map(image => Iterator.iterate(image)(_.enhance).drop(times).next)
          .map(_.pixels.count(_._2 == "1"))
      }

  def part1(inputPath:String) =
    solve(inputPath, 2)

  def part2(inputPath:String) =
    solve(inputPath, 50)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day20/day20-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day20/day20-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
