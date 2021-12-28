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
  val rolls = for( fst <- die ; sec <- die ; trd <- die ) yield fst + sec + trd

  type Pos = Long
  type Turn = Int
  type Score = Long
  type Universes = Long

  def playOneTurn(prevOutcomes:Map[(Pos,Score),Universes]):Map[(Pos,Score),Universes] =
    prevOutcomes.foldLeft(Map.empty[(Pos,Score),Universes]) {
      case (nextOutcomes, ((prevPos,prevScore),prevCount)) =>
        nextScores(prevPos).foldLeft(nextOutcomes) {
          case (acc, (nextPos, newCounts)) =>
            val nextScore = prevScore + nextPos
            val nextCount = prevCount * newCounts
            acc.updated((nextPos, nextScore), acc.getOrElse((nextPos, nextScore), 0L) + nextCount)
        }
    }

  def play(inProgress:Map[(Pos,Score),Universes], done:Map[Turn, Universes], turn:Int):Map[Turn, Universes] =
    if(inProgress.isEmpty) done
    else {
      val (completed, stillInProgress) = playOneTurn(inProgress).partition(_._1._2 >= 21)
      play(stillInProgress, if(completed.isEmpty) done else done.updated(turn, completed.values.sum), turn+1)
    }

  def mostWins(startPosP1: Pos, startPosP2: Pos) = {
    val outcomesP1 = play(Map((startPosP1, 0L) -> 1L), Map.empty, 0)
    val outcomesP2 = play(Map((startPosP2, 0L) -> 1L), Map.empty, 0)
    val winsP1 = for ( (tp1, unisp1) <- outcomesP1 ; (tp2, _) <- outcomesP2 if tp1 <= tp2) yield unisp1
    val winsP2 = for ( (tp2, unisp2) <- outcomesP2 ; (tp1, _) <- outcomesP2 if tp2 < tp1) yield unisp2
    (outcomesP1.values.sum, outcomesP2.values.sum)
  }


  def nextScores(p:Pos):Map[Pos, Universes] =
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
      part2("day21/day21-test-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
