package freskog.aoc.day3

import zio._
import zio.stream._

import freskog.aoc.utils._

import scala.collection.immutable._
import freskog.aoc.day3.Day3Solution.Trie.Node
import freskog.aoc.day3.Day3Solution.Trie.Empty

object Day3Solution extends ZIOAppDefault {

  sealed trait Trie { self =>

    def zeroes: Int
    def ones: Int
    def left: Trie
    def right: Trie

    def most: List[Int] =
      self match {
        case Trie.Node(zeroes, ones, left, right) => if (ones >= zeroes) 1 :: right.most else 0 :: left.most
        case Trie.Empty                           => Nil
      }

    def least: List[Int] =
      self match {
        case Node(0, _, _, right)            => 1 :: right.least
        case Node(_, 0, left, _)             => 0 :: left.least
        case Node(zeroes, ones, left, right) => if (zeroes <= ones) 0 :: left.least else 1 :: right.least
        case Empty                           => Nil
      }

    def add(input: List[Int]): Trie =
      input match {
        case Nil       => self
        case 0 :: next => Trie.Node(zeroes + 1, ones, left.add(next), right)
        case _ :: next => Trie.Node(zeroes, ones + 1, left, right.add(next))
      }

    def calculateLeastTimesMost: Int =
        Integer.parseInt(least.mkString, 2) * Integer.parseInt(most.mkString, 2)

  }
  object Trie {
    case class Node(zeroes: Int, ones: Int, left: Trie, right: Trie) extends Trie
    case object Empty                                                extends Trie {
      def zeroes: Int = 0
      def ones: Int   = 0
      def left: Trie  = Trie.Empty
      def right: Trie = Trie.Empty
    }

  }

  case class Counter(entries: Int, acc: Option[Array[Int]]) {
    def add(a1: Array[Int], a2: Array[Int]): Array[Int] =
      a1.zip(a2).map { case (n1, n2) => n1 + n2 }

    def updateWith(input: Array[Int]): Counter          =
      Counter(entries + 1, acc.map(add(_, input)).orElse(Some(input)))

    def calculateMostCommon: String =
      acc.map(_.map(n => if (n > entries / 2) "1" else "0").mkString).getOrElse("N/A")

    def calculateLeastCommon: String =
      acc.map(_.map(n => if (n < entries / 2) "1" else "0").mkString).getOrElse("N/A")

    def calculateMostTimesLeast: Int =
      Integer.parseInt(calculateLeastCommon, 2) * Integer.parseInt(calculateMostCommon, 2)
  }

  def parseInt(c: Char): Int =
    if (c == '0') 0 else 1

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(_.toArray.map(parseInt))
      .runFold(Counter(0, None))(_ updateWith _)
      .map(_.calculateMostTimesLeast)

  def part2(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(_.toList.map(parseInt))
      .runFold[Trie](Trie.Empty)(_ add _)
      .map(_.calculateLeastTimesMost)

  val run =
    part1("day3/day3-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
    part2("day3/day3-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))

}
