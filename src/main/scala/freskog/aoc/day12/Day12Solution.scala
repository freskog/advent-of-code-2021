package freskog.aoc.day12

import zio._

import freskog.aoc.utils._

object Day12Solution extends ZIOAppDefault {

  def go(
    curr: String,
    path: List[String],
    remaining: Map[String, Int],
    acc: Set[List[String]],
    caverns: Map[String, Set[String]]
  ): Set[List[String]] = {
    val next = caverns.getOrElse(curr, Set.empty).diff(remaining.filter(_._2 == 0).keys.toSet)
    if (curr == "end")
      acc + (curr :: path).reverse
    else if (next.isEmpty)
      acc
    else
      next.flatMap(next =>
        go(next, curr :: path, if (curr.forall(_.isLower)) remaining.updated(curr, remaining(curr) - 1) else remaining, acc, caverns)
      )
  }

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(pair =>
        pair.split('-').toList match {
          case src :: dest :: Nil => src -> dest
        }
      )
      .runFold(Map.empty[String, Set[String]]) { case (acc, (src, dest)) =>
        acc
          .updated(src, acc.getOrElse(src, Set.empty) + dest)
          .updated(dest, acc.getOrElse(dest, Set.empty) + src)
      }
      .map(caverns => go("start", Nil, caverns.collect { case (node, _) if node.forall(_.isLower) => (node, 1) }, Set.empty, caverns))
      .map(_.size)

  def part2(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(pair =>
        pair.split('-').toList match {
          case src :: dest :: Nil => src -> dest
        }
      )
      .runFold(Map.empty[String, Set[String]]) { case (acc, (src, dest)) =>
        acc
          .updated(src, acc.getOrElse(src, Set.empty) + dest)
          .updated(dest, acc.getOrElse(dest, Set.empty) + src)
      }
      .map { caverns =>
        val restricted =  caverns.keys.filter(_.forall(_.isLower)).map(_ -> 1).toMap
        restricted.flatMap(n =>
          go("start", Nil, if(n._1 != "end" && n._1 != "start") restricted.updated(n._1, 2) else restricted, Set.empty, caverns)
        ).toSet
      }
      .map(_.size)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day12/day12-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
    part2("day12/day12-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
