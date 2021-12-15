package freskog.aoc.day15

import zio._
import freskog.aoc.utils._

import scala.annotation.tailrec

object Day15Solution extends ZIOAppDefault {

  case class Loc(x: Int, y: Int)

  case class Cave(cost: Map[Loc, Int], endLoc: Loc) { self =>
    def adjacent(loc: Loc): Chunk[Loc] =
      Chunk(Loc(loc.x - 1, loc.y), Loc(loc.x, loc.y - 1), Loc(loc.x + 1, loc.y), Loc(loc.x, loc.y + 1)).filter(cost.contains)

    def riskOf(path: Chunk[Loc]): Int =
      path.map(cost).sum

    def calculateCost(n: Loc, prev: Map[Loc, Loc], acc: Int): Int =
      if (prev.contains(n)) calculateCost(prev(n), prev, acc + cost(n)) else acc

    def calculatePath(n: Loc, prev: Map[Loc, Loc], acc: Chunk[Loc]): Chunk[Loc] =
      if (prev.contains(n)) calculatePath(prev(n), prev, acc.prepended(n)) else acc

    @tailrec
    private def dijkstra(
      current: Loc,
      dest: Loc,
      prev: Map[Loc, Loc],
      dist: Map[Loc, Int],
      unvisitedDist: Map[Loc, Int],
      unvisited: Set[Loc]
    ): Chunk[Loc] =
      if (unvisited.isEmpty) Chunk.empty
      else if (current == dest) calculatePath(dest, prev, Chunk.empty)
      else {
        val (nextDist, nextPrev, nextUnvisitedDist) =
          adjacent(current)
            .filter(unvisited.contains)
            .foldLeft((dist, prev, unvisitedDist)) { case ((distAcc, prevAcc, unvisitedDistAcc), neighbor) =>
              val alt = dist(current) + cost(neighbor)
              if (alt < distAcc.getOrElse(neighbor, Int.MaxValue))
                (distAcc.updated(neighbor, alt), prevAcc.updated(neighbor, current), unvisitedDistAcc.updated(neighbor, alt))
              else
                (distAcc, prevAcc, unvisitedDistAcc)
            }
        val (next, _)                               = nextUnvisitedDist.minBy(_._2)
        dijkstra(next, dest, nextPrev, nextDist, nextUnvisitedDist - current, unvisited - current)
      }

    def safestPath: Chunk[Loc] =
      dijkstra(Loc(0, 0), endLoc, Map.empty, Map(Loc(0, 0) -> 0), Map.empty, cost.keySet - Loc(0, 0))

    def render: String         =
      "\n" ++
        (for (y <- 0 to endLoc.y) yield (for (x <- 0 to endLoc.x) yield cost(Loc(x, y))).mkString("")).mkString("\n")

  }

  object Cave {
    def from(input: String): Cave = {
      val (Loc(x, y), cost) = input.trim.foldLeft((Loc(0, 0), Map.empty[Loc, Int])) {
        case ((l, acc), '\n') => (Loc(0, l.y + 1), acc)
        case ((l, acc), n)    => (Loc(l.x + 1, l.y), acc.updated(l, n.toString.toInt))
      }
      Cave(cost, Loc(x - 1, y))
    }
    def expandFrom(input: String): Cave = {
      @tailrec
      def shift(n: Int, steps: Int): Int = if (steps == 0) n else shift(if (n == 9) 1 else n + 1, steps - 1)
      val tile                           = from(input)
      val xOffsets                       = (0 until 5).map(n => (n * (tile.endLoc.x + 1), n)).toSet
      val yOffsets                       = (0 until 5).map(n => (n * (tile.endLoc.y + 1), n)).toSet
      Cave(
        for {
          (loc, v) <- tile.cost
          (x, xn)  <- xOffsets
          (y, yn)  <- yOffsets
        } yield Loc(loc.x + x, loc.y + y) -> shift(v, xn + yn),
        Loc(tile.endLoc.x + (4 * (tile.endLoc.x + 1)), tile.endLoc.y + (4 * (tile.endLoc.y + 1)))
      )
    }
  }

  def part1(inputPath: String) =
    readAllAsString(inputPath).map(Cave.from).map(c => c.safestPath.map(c.cost).sum)

  def part2(inputPath: String) =
    readAllAsString(inputPath).map(Cave.expandFrom).map(c => c.safestPath.map(c.cost).sum)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day15/day15-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day15/day15-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
