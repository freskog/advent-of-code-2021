package freskog.aoc.day21

import zio._

import freskog.aoc.utils._

object Day21Solution extends ZIOAppDefault {

  case class Board(p1:Long, p2:Long, nextUp:Int, scorep1:Long, scorep2:Long, lastDie:Long, turns:Long) {
    def takeTurn:Board = {

      val p = if(nextUp == 1) p1 else p2
      val prevScore = if(nextUp == 1) scorep1 else scorep2
      val newP = (p + ((lastDie * 3L) + 1L + 2L + 3L)) % 10L
      val newScore = prevScore + pos(newP)
      Board(
        if(nextUp == 1) newP else p1,
        if(nextUp == 2) newP else p2,
        if(nextUp == 1) 2 else 1,
        if(nextUp == 1) newScore else scorep1,
        if(nextUp == 2) newScore else scorep2,
        (lastDie + 3) % 100,
        turns + 3L
      )
    }
    def answerPart1 = turns * math.min(scorep1, scorep2)
  }

  def pos(n:Long):Score = if (n == 0L) 10L else n

  val die = List(1L,2L,3L)
  val rolls = (for( fst <- die ; sec <- die ; trd <- die ) yield fst + sec + trd)

  type Pos = Long
  type Turn = Int
  type Score = Long
  type Universes = Long

  def combine(t1:(Long,Long),t2:(Long,Long)):(Long,Long) =
    (t1._1 + t2._1, t1._2 + t2._2)

  def solvePart2(p1:Pos, p2:Pos, scoreP1:Score, scoreP2:Score, unis:Universes, nextUp:Int):(Universes, Universes) =
    if(scoreP1 >= 21) (unis, 0L)
    else if(scoreP2 >= 21) (0L, unis)
    else if(nextUp == 1)
      nextPos(p1).foldLeft((0L,0L)) {
        case (acc, (nextP1,unisP1)) =>
          combine(acc, solvePart2(nextP1,p2, scoreP1+nextP1, scoreP2, unis*unisP1, 2))
      }
    else
      nextPos(p2).foldLeft((0L,0L)) {
        case (acc, (nextP2,unisP2)) =>
          combine(acc,solvePart2(p1, nextP2, scoreP1, scoreP2+nextP2, unis*unisP2, 1))
      }

  def mostWins(l: Long, l1: Long):Long = {
    val (p1, p2) = solvePart2(l,l1, 0L, 0L, 1L, 1)
    math.max(p1,p2)
  }


  def nextPos(p:Pos):Map[Pos, Universes] =
    rolls.groupBy(identity).map { case (sum, outcomes) => pos((sum + p) % 10L) -> outcomes.size.toLong }

  val inputPattern = """Player 1 starting position: (\d+)
                       |Player 2 starting position: (\d+)""".stripMargin.r

  def solve(inputPath:String) =
    readAllAsString(inputPath).map {
      case inputPattern(p1,p2) =>
        Iterator
          .iterate(Board(if(p1 == "10") 0 else p1.toLong,if(p2 == "10") 0 else p2.toLong,1,0,0,0,0))(_.takeTurn)
          .dropWhile(b => b.scorep1 < 1000 && b.scorep2 < 1000)
          .next()
    }

  def part1(inputPath:String) =
    solve(inputPath).map(_.answerPart1)

  def part2(inputPath:String) =
    readAllAsString(inputPath).map {
      case inputPattern(p1, p2) =>
        mostWins(if(p1 == "10") 0L else p1.toLong, if(p2 == "10") 0L else p2.toLong)
    }

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day21/day21-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day21/day21-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
