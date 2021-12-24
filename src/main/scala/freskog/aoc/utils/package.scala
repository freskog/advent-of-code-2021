package freskog.aoc

import zio.{Chunk, ZIO}
import zio.stream.ZPipeline._
import zio.stream._

package object utils {

  def readAllAsString[A](inputPath:String):ZIO[Any, Throwable, String] =
    (ZStream.fromResource(inputPath) via utf8Decode).runFold("")(_ ++ _)

  def readAsOneStringPerLine(inputPath:String):ZStream[Any, Throwable, String] =
    ZStream.fromResource(inputPath) via (utf8Decode andThen splitLines)

  def readAsGroupsSplitByEmptyLine(inputPath:String):ZStream[Any, Throwable, String] =
    ZStream.fromResource(inputPath) via (utf8Decode andThen splitOn("\n\n"))

  def readAsOneLongPerLine(inputPath:String):ZStream[Any, Throwable, Long] =
    readAsOneStringPerLine(inputPath).mapZIO(num => ZIO.attempt(num.toLong))

  def sliding(n:Int) =
    scan[Long, Chunk[Long]](Chunk.empty) {
      case (acc, a) => if(acc.size < n) acc.appended(a) else acc.drop(1).appended(a)
    } andThen dropWhile[Chunk[Long]](_.size < n)

}
