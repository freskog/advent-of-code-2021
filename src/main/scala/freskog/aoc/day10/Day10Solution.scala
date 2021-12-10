package freskog.aoc.day10

import zio._
import freskog.aoc.utils._

import scala.annotation.tailrec

object Day10Solution extends ZIOAppDefault {

  @tailrec
  def checkSyntax(input: Iterator[Char], nextClose: List[Char]): Either[Char, List[Char]] =
    if (input.isEmpty) Right(nextClose)
    else
      (input.next(), nextClose) match {
        case ('(', _)                       => checkSyntax(input, ')' :: nextClose)
        case ('{', _)                       => checkSyntax(input, '}' :: nextClose)
        case ('<', _)                       => checkSyntax(input, '>' :: nextClose)
        case ('[', _)                       => checkSyntax(input, ']' :: nextClose)
        case (c, x :: remToClose) if c == x => checkSyntax(input, remToClose)
        case (c, _)                         => Left(c)
      }

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(input => checkSyntax(input.iterator, Nil).swap.toOption)
      .collectSome
      .runFold(Map.empty[Char, Long]) {
        case (acc, ')') => acc.updated(')', acc.getOrElse(')', 0L) + 3L)
        case (acc, ']') => acc.updated(']', acc.getOrElse(']', 0L) + 57L)
        case (acc, '}') => acc.updated('}', acc.getOrElse('}', 0L) + 1197L)
        case (acc, '>') => acc.updated('>', acc.getOrElse('>', 0L) + 25137L)
      }
      .map(_.values.sum)

  def part2(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(input => checkSyntax(input.iterator, Nil).toOption)
      .collectSome
      .map {
        _.foldLeft(0L) {
          case (acc, ')') => (acc * 5L) + 1L
          case (acc, ']') => (acc * 5L) + 2L
          case (acc, '}') => (acc * 5L) + 3L
          case (acc, '>') => (acc * 5L) + 4L
        }
      }
      .runCollect
      .map(scores => scores.sorted.apply(scores.length / 2))

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day10/day10-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day10/day10-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
