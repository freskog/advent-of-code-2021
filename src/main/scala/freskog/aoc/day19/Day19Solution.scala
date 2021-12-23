package freskog.aoc.day19

import zio._
import freskog.aoc.utils._
import zio.stream.ZSink

object Day19Solution extends ZIOAppDefault {

  case class Pos(x: Int, y: Int, z: Int) { self =>
    def -(other: Pos): Pos =
      Pos(self.x - other.x, self.y - other.y, self.z - other.z)
    def +(other: Pos): Pos =
      Pos(self.x + other.x, self.y + other.y, self.z + other.z)
    def dist(other: Pos): Int = {
      def manhattan(x1: Int, x2: Int): Int =
        if ((x1 < 0 && x2 > 0) || (x1 > 0 && x2 < 0)) math.abs(x1) + math.abs(x2)
        else math.abs(math.abs(x1) - math.abs(x2))
      manhattan(self.x, other.x) + manhattan(self.y, other.y) + manhattan(self.z, other.z)
    }
  }
  object Pos                             {
    def from(input: String): LazyList[Pos] =
      input
        .split('\n')
        .map(
          _.split(",").toList match {
            case x :: y :: z :: Nil => Pos(x.toInt, y.toInt, z.toInt)
          }
        )
        .to(LazyList)
  }

  case class Scanner(pos: Pos, relBeacons: LazyList[Pos], beacons: Set[Pos]) { self =>

    def sharedBeacons(other: Scanner): Set[Pos] = {
      def checkDist(p1: Pos, p2: Pos): Boolean =
        math.abs(p1.x - p2.x) <= 1000 && math.abs(p1.y - p2.y) <= 1000 && math.abs(p1.z - p2.z) <= 1000
      self.beacons.filter(checkDist(other.pos, _)) ++ other.beacons.filter(checkDist(self.pos, _))
    }

    def nextScanner(otherBeacons: LazyList[Pos], matches: Int): Option[Scanner] = {

      def check(rot: Pos => Pos, proposedPos: Pos): Boolean =
        otherBeacons.count(otherPos => relBeacons.contains(proposedPos + rot(otherPos))) >= matches

      def proposedRelPos(rotation: Pos => Pos) = for {
        x1 <- relBeacons
        x2 <- otherBeacons.map(rotation)
      } yield x1 - x2

      allRotations
        .flatMap(rot => proposedRelPos(rot).find(check(rot, _)).map((rot, _)))
        .map { case (rot, relPos) =>
          Scanner(relPos + pos, otherBeacons.map(rot), otherBeacons.toSet[Pos].map(beacon => rot(beacon) + relPos + pos))
        }
        .headOption
    }
  }

  def findScannerFromBeacons(scanners: LazyList[Scanner], beacons: LazyList[Pos]): Option[Scanner] =
    scanners.map(_.nextScanner(beacons, 12)).find(_.isDefined).flatten

  def findAllScanners(scanners: LazyList[Scanner], allRelBeacons: List[LazyList[Pos]]): List[Scanner] =
    if (allRelBeacons.isEmpty) scanners.toList
    else {
      val nextScanner = findScannerFromBeacons(scanners, allRelBeacons.head)
      findAllScanners(
        nextScanner.fold(scanners)(_ +: scanners),
        if (nextScanner.isDefined) allRelBeacons.tail else allRelBeacons.tail ::: List(allRelBeacons.head)
      )
    }

  def countUnique(scanners: List[Scanner]): Int =
    scanners.foldLeft(Set.empty[Pos])(_ ++ _.beacons).size

  val rotateFwdZ: Pos => Pos = { case Pos(x: Int, y: Int, z: Int) => Pos(y, -x, z) }
  val rotateFwdY: Pos => Pos = { case Pos(x: Int, y: Int, z: Int) => Pos(z, y, -x) }
  val rotateFwdX: Pos => Pos = { case Pos(x: Int, y: Int, z: Int) => Pos(x, z, -y) }
  val rotateBwdZ: Pos => Pos = { case Pos(x: Int, y: Int, z: Int) => Pos(-y, x, z) }
  val rotateBwdY: Pos => Pos = { case Pos(x: Int, y: Int, z: Int) => Pos(-z, y, x) }
  val rotateBwdX: Pos => Pos = { case Pos(x: Int, y: Int, z: Int) => Pos(x, -z, y) }

  val allRotations: LazyList[Pos => Pos] =
    LazyList(
      rotateFwdY,
      rotateFwdY andThen rotateFwdY,
      rotateFwdY andThen rotateFwdY andThen rotateFwdY,
      rotateFwdY andThen rotateFwdY andThen rotateFwdY andThen rotateFwdY,
      rotateFwdX andThen rotateFwdX andThen rotateFwdY,
      rotateFwdX andThen rotateFwdX andThen rotateFwdY andThen rotateFwdY,
      rotateFwdX andThen rotateFwdX andThen rotateFwdY andThen rotateFwdY andThen rotateFwdY,
      rotateFwdX andThen rotateFwdX andThen rotateFwdY andThen rotateFwdY andThen rotateFwdY andThen rotateFwdY,
      rotateFwdX andThen rotateFwdZ,
      rotateFwdX andThen rotateFwdZ andThen rotateFwdZ,
      rotateFwdX andThen rotateFwdZ andThen rotateFwdZ andThen rotateFwdZ,
      rotateFwdX andThen rotateFwdZ andThen rotateFwdZ andThen rotateFwdZ andThen rotateFwdZ,
      rotateBwdX andThen rotateFwdZ,
      rotateBwdX andThen rotateFwdZ andThen rotateFwdZ,
      rotateBwdX andThen rotateFwdZ andThen rotateFwdZ andThen rotateFwdZ,
      rotateBwdX andThen rotateFwdZ andThen rotateFwdZ andThen rotateFwdZ andThen rotateFwdZ,
      rotateFwdZ andThen rotateFwdX,
      rotateFwdZ andThen rotateFwdX andThen rotateFwdX,
      rotateFwdZ andThen rotateFwdX andThen rotateFwdX andThen rotateFwdX,
      rotateFwdZ andThen rotateFwdX andThen rotateFwdX andThen rotateFwdX andThen rotateFwdX,
      rotateBwdZ andThen rotateFwdX,
      rotateBwdZ andThen rotateFwdX andThen rotateFwdX,
      rotateBwdZ andThen rotateFwdX andThen rotateFwdX andThen rotateFwdX,
      rotateBwdZ andThen rotateFwdX andThen rotateFwdX andThen rotateFwdX andThen rotateFwdX
    )

  def dropFirstLine(text: String) =
    text.split("\n").tail.mkString("\n")

  def parseScanners(inputPath: String) =
    readAsGroupsSplitByEmptyLine(inputPath)
      .map(block => Pos.from(dropFirstLine(block)))
      .run(ZSink.collectAll)
      .map { beacons =>
        findAllScanners(LazyList(Scanner(Pos(0, 0, 0), beacons.head, beacons.head.toSet)), beacons.tail.toList)
      }

  def longestDist(scanners: List[Scanner]): Int = {
    val positions = scanners.map(_.pos)
    val dists     =
      for {
        pos1 <- positions
        pos2 <- positions.filterNot(_ == pos1)
      } yield pos1.dist(pos2)
    dists.max
  }

  def part1(inputPath: String) =
    parseScanners(inputPath).map(countUnique)

  def part2(inputPath: String) =
    parseScanners(inputPath).map(longestDist)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day19/day19-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer")) *>
      part2("day19/day19-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
