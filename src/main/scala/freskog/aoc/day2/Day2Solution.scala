package freskog.aoc.day2

import zio._
import zio.stream._

import freskog.aoc.utils._
import java.text.Normalizer.Form

object Day2Solution extends ZIOAppDefault {

  sealed trait Direction
  object Direction   {
    def apply(direction: String): Option[Direction] =
      direction match {
        case "forward" => Some(Forward)
        case "up"      => Some(Up)
        case "down"    => Some(Down)
        case _         => None
      }

    case object Up      extends Direction
    case object Down    extends Direction
    case object Forward extends Direction
  }
  case class Instruction(direction: Direction, units: Int)
  object Instruction {
    def apply(input: String): Option[Instruction] = {
      val pattern = """(\w+) (\d+)""".r
      input match {
        case pattern(direction, units) =>
          Direction(direction).zip(units.toIntOption).map { case (d, u) => Instruction(d, u) }
        case _                         => None
      }
    }
  }

  case class Pos(horizontal: Int, depth: Int) {
    def update(instruction: Instruction): Pos =
      instruction match {
        case Instruction(Direction.Up, units)      => Pos(horizontal, depth - units)
        case Instruction(Direction.Down, units)    => Pos(horizontal, depth + units)
        case Instruction(Direction.Forward, units) => Pos(horizontal + units, depth)
      }
  }

  case class PosWithAim(horizontal:Int, depth: Int, aim:Int) {
      def update(instruction:Instruction): PosWithAim =
          instruction match {
              case Instruction(Direction.Up, units) => PosWithAim(horizontal, depth, aim - units)
              case Instruction(Direction.Down, units) => PosWithAim(horizontal, depth, aim + units)
              case Instruction(Direction.Forward, units) => PosWithAim(horizontal + units, depth + (aim * units), aim)
          }
  }

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath)
        .mapZIO((in:String) => ZIO.fromOption(Instruction(in)).mapError( _ => new IllegalArgumentException(in)))
        .runFold(Pos(0,0))(_ update _)
        .map(pos => pos.horizontal * pos.depth)

  def part2(inputPath:String) =
      readAsOneStringPerLine(inputPath)
        .mapZIO((in:String) => ZIO.fromOption(Instruction(in)).mapError(_ => new IllegalArgumentException(in)))
        .runFold(PosWithAim(0,0,0))(_ update _)
        .map(pos => pos.horizontal * pos.depth)

  def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] = 
      part1("day2/day2-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
      part2("day2/day2-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))

}
