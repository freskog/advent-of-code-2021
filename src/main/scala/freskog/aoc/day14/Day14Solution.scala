package freskog.aoc.day14

import zio._
import freskog.aoc.utils._
import zio.stream.{ ZPipeline, ZSink }

object Day14Solution extends ZIOAppDefault {

  def unfold(
    pairs: Map[(Char, Char), Long],
    counts: Map[Char, Long],
    polymers: Map[(Char, Char), Char]
  ): (Map[(Char, Char), Long], Map[Char, Long]) = {
    pairs.foldLeft((Map.empty[(Char, Char), Long],counts)) {
      case ((acc, newCount), (pair, pairCount)) =>
        val nxt1 = (pair._1, polymers(pair))
        val nxt2 = (polymers(pair), pair._2)
        (acc
          .updated(nxt1, acc.getOrElse(nxt1,0L) + pairCount)
          .updated(nxt2, acc.getOrElse(nxt2,0L) + pairCount),
         newCount
           .updated(nxt1._2, newCount.getOrElse(nxt1._2,0L)+pairCount)
        )
    }
  }

  def solve(inputPath: String, steps:Int) =
    readAsGroupsSplitByEmptyLine(inputPath)
      .peel(ZSink.take[Throwable, String](1).map(_.head).map(_.toCharArray).map(Chunk.fromArray))
      .use({ case (start, mappings) =>
        val pairPattern = """(\w+) -> (\w)""".r
        mappings
          .via(ZPipeline.splitLines)
          .collect { case pairPattern(src, dest) =>
            val srcChk  = (src.charAt(0), src.charAt(1))
            srcChk -> dest.head
          }
          .runFold(Map.empty[(Char, Char), Char])((acc, pair) => acc + pair)
          .map { mappings =>
            val initialCount = start.foldLeft(Map.empty[Char, Long])((acc, c) => acc.updated(c, acc.getOrElse(c, 0L) + 1))
            val initialPairs =
              start
                .sliding(2)
                .takeWhile(_.size == 2)
                .map(chk => (chk.head, chk.last))
                .foldLeft(Map.empty[(Char, Char), Long]) { case (acc, pair) =>
                  acc.updated(pair, acc.getOrElse(pair, 0L) + 1L)
                }

            val (_,result) = Iterator
              .iterate((initialPairs, initialCount)) {
                case (pairs, counts) => unfold(pairs, counts, mappings)
              }
              .drop(steps)
              .next()
            val smallest:Long = result.minByOption(_._2).map(_._2).getOrElse(0)
            val largest:Long  = result.maxByOption(_._2).map(_._2).getOrElse(0)
            largest - smallest
          }
      })

  def part1(inputPath: String) =
    solve(inputPath,10)

  def part2(inputPath:String) =
    solve(inputPath, 40)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day14/day14-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day14/day14-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
