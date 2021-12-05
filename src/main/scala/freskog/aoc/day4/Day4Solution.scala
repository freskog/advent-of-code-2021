package freskog.aoc.day4

import zio._
import zio.stream._
import java.io.IOException

object Day4Solution extends ZIOAppDefault {

  case class BingoCard(markedNumbers: List[Int], byRows: Array[Array[Int]], byCols: Array[Array[Int]]) {

    def hasBingo: Boolean =
      byRows.exists(_.forall(markedNumbers.contains)) || byCols.exists(_.forall(markedNumbers.contains))

    def addNumber(n: Int): BingoCard =
      BingoCard(n :: markedNumbers, byRows, byCols)

    def score: Int =
      byRows.flatMap(_.filterNot(n => markedNumbers.contains(n))).sum * markedNumbers.head

    def play(input:List[Int]):Option[BingoCard] =
      if(input.isEmpty) None
      else {
        val newCard = addNumber(input.head)
        if(newCard.hasBingo) Some(newCard)
        else newCard.play(input.tail)
      }

    def render:String =
        s"""|
            |byRows:
            |${byRows.map(_.mkString(",")).mkString("\n")}
            |
            |byCols:
            |${byCols.map(_.mkString(",")).mkString("\n")}
            |""".stripMargin
  }

  object BingoCard {
    def buildBingoCard(input: String): BingoCard =
      BingoCard(
        Nil,
        input.trim.split("\n").map(_.split(" ").filter(_.nonEmpty).map(_.toInt)),
        input.trim.split("\n").map(_.split(" ").filter(_.nonEmpty).map(_.toInt)).transpose
      )
  }

  def readBingoCard(path: String, winner:Boolean) =
    ZStream
      .fromResource(path)
      .via(ZPipeline.utf8Decode @@ ZPipeline.splitOn("\n\n"))
      .filterNot(_.isEmpty())
      .peel(ZSink.take[IOException, String](1).map(_.mkString.split(",").toList.map(_.toInt)))
      .use {
          case (numbers, cards) => 
              cards
              .map(BingoCard.buildBingoCard(_))
              .map(_.play(numbers))
              .collect { case Some(bingoCard) => bingoCard }
              .runFold(BingoCard(if(winner) numbers else Nil, new Array[Array[Int]](0), new Array[Array[Int]](0)))(
                  (smallest, current) => 
                      if(winner) 
                        if(current.markedNumbers.length < smallest.markedNumbers.length) current else smallest
                      else
                        if(current.markedNumbers.length > smallest.markedNumbers.length) current else smallest
              ).map(_.score)             
      }

  def part1(path:String) =
      readBingoCard(path, true) 

  def part2(path:String) =
      readBingoCard(path, false)
  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
      part1("day4/day4-input-part-1.txt").flatMap( answer => Console.printLine(s"Part1: $answer")) *>
      part2("day4/day4-input-part-1.txt").flatMap( answer => Console.printLine(s"Part2: $answer"))

}
