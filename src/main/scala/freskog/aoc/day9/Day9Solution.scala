package freskog.aoc.day9

import zio._
import freskog.aoc.utils._

import scala.annotation.tailrec

object Day9Solution extends ZIOAppDefault {

  case class Location(x:Int, y:Int)

  case class HeightMap(byRows:Array[Array[Int]]) {

    def allLocation:Set[Location] =
      selectByLocation(_ => true).toSet

    @tailrec
    final def exploreBasin(remaining:List[Location], visited:Set[Location]):Set[Location] =
      if(remaining.isEmpty) visited
      else {
        val next = neighborsOf(remaining.head).filter(l => !visited.contains(l) && valueAt(l) != 9)
        exploreBasin(remaining.tail ++ next, visited + remaining.head)
      }

    def findBasins(uncategorized:Set[Location], found:List[Set[Location]]):List[Set[Location]] = {
      val inBasin = uncategorized.find(valueAt(_) < 9)
      if(inBasin.isEmpty) found
      else {
        val basin = exploreBasin(List(inBasin.get), Set.empty)
        findBasins(uncategorized -- basin, basin :: found)
      }
    }

    def neighborsOf(l:Location):List[Location] =
      (if (l.x > 0) List(Location(l.x - 1, l.y)) else Nil) ++
      (if (l.y > 0) List(Location(l.x, l.y - 1)) else Nil) ++
      (if (l.x < byRows(l.y).length-1) List(Location(l.x + 1, l.y)) else Nil) ++
      (if (l.y < byRows.length-1) List(Location(l.x, l.y + 1)) else Nil)

    def selectByLocation(p:Location => Boolean):List[Location] =
      byRows.indices.flatMap(y => byRows(y).indices.collect {
        case x if p(Location(x,y)) => Location(x,y)
      }).toList

    def valueAt(location:Location):Int =
      byRows(location.y)(location.x)
  }

  def part1(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .map(_.map { c => c.toString.toInt }.toArray)
      .runFold(Array.empty[Array[Int]])(_ :+ _)
      .map { byRows =>
        val hmap = HeightMap(byRows)
        hmap.selectByLocation(
          l => hmap.neighborsOf(l).map(hmap.valueAt).forall(_ > hmap.valueAt(l))
        ).map(hmap.valueAt(_) + 1).sum
      }

  def part2(inputPath:String) =
    readAsOneStringPerLine(inputPath)
      .map(_.map { c => c.toString.toInt }.toArray)
      .runFold(Array.empty[Array[Int]])(_ :+ _)
      .map { byRows =>
        val hmap = HeightMap(byRows)
        hmap
          .findBasins(hmap.allLocation, Nil)
          .map(_.size)
          .sorted
          .reverse
          .take(3)
          .product
      }

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day9/day9-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
    part2("day9/day9-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))

}
