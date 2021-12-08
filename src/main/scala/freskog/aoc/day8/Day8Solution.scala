package freskog.aoc.day8

import zio._
import freskog.aoc.utils._


object Day8Solution extends ZIOAppDefault {

  // 1,4,7,8 have unique counts
  // 1 - 2 wires
  // 4 - 4 wires
  // 7 - 3 wires
  // 8 - 7 wires

  def decode4(number:Set[Char]) =
    number.size match {
      case 2 => Some(1)
      case 4 => Some(4)
      case 3 => Some(7)
      case 7 => Some(8)
      case _ => None
    }

  // 2,5,3 - 5 wires
  // of 2,5,3 -> 2 - 4 = 3 remaining wires
  // of 2,5,3 -> 5 - 4 = 2 remaining wires
  // of 2,5,3 -> 3 - 4 = 2 remaining wires
  // of 2,5,3 -> 5 - 1 = 4 remaining wires
  // of 2,5,3 -> 3 - 1 = 3 remaining wires

  // 0,6,9 - 6 wires
  // of 0,6,9 -> 9 - 4 = 2 remaining wire
  // of 0,6,9 -> 6 - 4 = 3 remaining wires
  // of 0,6,9 -> 0 - 4 = 3 remaining wires
  // of 0,6,9 -> 0 - 1 = 4 remaining wires
  // of 0,6,9 -> 6 - 1 = 5 remaining wires

  // Decode any number
  def decode(input:Set[Set[Char]]):Map[Set[Char],Int] = {
    val decoded = input.collect {
      case str if decode4(str).isDefined => (str, decode4(str).get)
    }.toMap
    val four = decoded.find(_._2 == 4).get._1
    val one = decoded.find(_._2 == 1).get._1
    val unknown = input -- decoded.keys.toSet
    unknown.foldLeft(decoded) {
      case (acc, str) if str.size == 5 && (str -- four).size == 3 => acc.updated(str, 2)
      case (acc, str) if str.size == 5 && (str -- four).size == 2 && (str -- one).size == 4 => acc.updated(str, 5)
      case (acc, str) if str.size == 5 && (str -- four).size == 2 && (str -- one).size == 3 => acc.updated(str, 3)
      case (acc, str) if str.size == 6 && (str -- four).size == 2 => acc.updated(str, 9)
      case (acc, str) if str.size == 6 && (str -- four).size == 3 && (str -- one).size == 4 => acc.updated(str, 0)
      case (acc, str) if str.size == 6 && (str -- four).size == 3 && (str -- one).size == 5 => acc.updated(str, 6)
      case (acc, str) => throw new IllegalArgumentException(s"$acc: found bad input $str")
    }
  }

  def part1(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .map( entry => entry.split('|').last.trim.split(' ').map(_.toSet).map(decode4).toList)
      .runFold(0)((acc, numbers) => acc + numbers.count(_.isDefined))

  def part2(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .map { entry =>
        val Array(signals, values) = entry.split('|')
        val decoded = decode(signals.trim.split(' ').map(_.toSet).toSet)
        val value = values.trim.split(' ').map(_.toSet).map(decoded(_)).mkString.toInt
        value
      }.runSum

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day8/day8-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
    part2("day8/day8-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))
}
