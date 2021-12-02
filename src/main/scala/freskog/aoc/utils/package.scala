package freskog.aoc

import zio.{Chunk, ZIO}
import zio.stream.ZPipeline._
import zio.stream.ZStream

package object utils {

  def readAsOneStringPerLine(inputPath:String):ZStream[Any, Throwable, String] =
    (ZStream.fromResource(inputPath) @@ utf8Decode @@ splitLines)

  def readAsOneLongPerLine(inputPath:String):ZStream[Any, Throwable, Long] =
    readAsOneStringPerLine(inputPath).mapZIO(num => ZIO.attempt(num.toLong))

  def sliding(n:Int) =
    scan[Long, Chunk[Long]](Chunk.empty) {
      case (acc, a) => if(acc.size < n) acc.appended(a) else acc.drop(1).appended(a)
    } @@ dropWhile[Chunk[Long]](_.size < n)

}
