package freskog.aoc.day1

import zio._
import zio.stream.ZPipeline._

import freskog.aoc.utils._

object Day1Solution extends ZIOAppDefault {

    val increasingOnly =
        collect[Chunk[Long],(Long,Long)] { case Chunk(prev, curr) if prev < curr => (prev,curr) }
    
    val part1 =
        sliding(2) @@ increasingOnly

    val part2 =
        sliding(3) @@ map[Chunk[Long],Long](_.sum) @@ sliding(2) @@ increasingOnly

    val run = {
        val input = readAsOneLongPerLine("day1/day1-input-part-1.txt")
        for {
            answer1 <- part1(input).runCount
            answer2 <- part2(input).runCount
            _ <- Console.printLine(s"Part1: $answer1 changes")
            _ <- Console.printLine(s"Part2: $answer2 changes")
        } yield ()
    }
}


