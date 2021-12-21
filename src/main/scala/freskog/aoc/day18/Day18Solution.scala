package freskog.aoc.day18

import fastparse.{ parse, Parsed }
import freskog.aoc.day18.Day18Solution.Number.NumberParser.number
import freskog.aoc.day18.Day18Solution.Number.{ Lit, Pair }
import freskog.aoc.utils.Zipper.{ Lens, Navigate }
import freskog.aoc.utils._
import zio._
import zio.stream.ZSink

object Day18Solution extends ZIOAppDefault {

  sealed trait Number { self =>

    def magnitude: Long =
      self match {
        case Number.Lit(n) => n
        case Pair(l, r)    => l.magnitude * 3L + r.magnitude * 2L
      }

    def asPair: Option[Pair] = self match {
      case Lit(_)     => None
      case Pair(l, r) => Some(Pair(l, r))
    }

    def asLit: Option[Lit] = self match {
      case Lit(n)     => Some(Lit(n))
      case Pair(_, _) => None
    }

    override def toString: String =
      self match {
        case Lit(n)     => n.toString
        case Pair(l, r) => s"[${l.toString}, ${r.toString}]"
      }
  }

  object Number {
    final case class Lit(n: Long)               extends Number
    final case class Pair(l: Number, r: Number) extends Number

    object NumberParser {
      import fastparse._, NoWhitespace._
      def lit[_: P]: P[Lit]       = CharPred(_.isDigit).!.map(_.toLong).map(Lit)
      def pair[_: P]: P[Pair]     = P("[" ~ number ~ "," ~ number ~ "]").map { case (l, r) => Pair(l, r) }
      def number[_: P]: P[Number] = pair | lit
    }
    def from(input: String): Number = {
      val Parsed.Success(num, _) = parse(input, number(_))
      num
    }
  }

  def shouldExplode(z: Zipper[Number, Pair, Lit]): Boolean =
    z.isNode && z.parents.length >= 4

  def shouldSplit(z: Zipper[Number, Pair, Lit]): Boolean                                     =
    z.isLeaf && (z.current match { case Lit(n) => n >= 10L })

  val explodeIfNeeded: PartialFunction[Zipper[Number, Pair, Lit], Zipper[Number, Pair, Lit]] = {
    case z if shouldExplode(z) => explode(z)
  }

  val split: PartialFunction[Zipper[Number, Pair, Lit], Zipper[Number, Pair, Lit]] = {
    case z if shouldSplit(z) =>
      val Lit(n) = z.current
      val res    = z.set(Pair(Lit(n / 2L), Lit(if (n % 2L == 1L) (n / 2L) + 1L else n / 2L)))
      if (shouldExplode(res)) explode(res) else res
  }

  def explode(z: Zipper[Number, Pair, Lit]): Zipper[Number, Pair, Lit] = {
    val Pair(Lit(l), Lit(r)) = z.current
    val step1                = z.set(Lit(0))
    val processNext          = step1.nextLeaf.map(add(_, r)).flatMap(_.prevLeaf)
    val processPrev          = processNext.getOrElse(step1).prevLeaf.map(add(_, l))
    processPrev.getOrElse(processNext.getOrElse(step1))
  }

  def add(z: Zipper[Number, Pair, Lit], n: Long): Zipper[Number, Pair, Lit] =
    z.current match { case Lit(prevN) => z.set(Lit(prevN + n)) }

  def reduce(num: Number): Number = {
    val zippedNumber = Zipper(
      num,
      Navigate[Number, Pair, Lit](
        leftL = Lens[Pair, Number](_.l, (p, n) => p.copy(l = n)),
        rightL = Lens[Pair, Number](_.r, (p, n) => p.copy(r = n)),
        asNode = _.asPair,
        asLeaf = _.asLit
      )
    )
    zippedNumber
      .transform(explodeIfNeeded) // Start by exploding all numbers
      .transform(split)           // Split, and maintain invariant around explode/split order
      .current
  }

  def addNumbers(first: Number, second: Number): Number =
    reduce(Pair(first, second))

  def part1(inputPath: String) =
    readAsOneStringPerLine(inputPath)
      .map(Number.from)
      .run(ZSink.collectAll)
      .map(_.reduce(addNumbers))
      .map(_.magnitude)

  def part2(inputPath: String) =
    (for {
      l <- readAsOneStringPerLine(inputPath).map(Number.from)
      r <- readAsOneStringPerLine(inputPath).map(Number.from).filterNot(_ == l)
    } yield addNumbers(l, r).magnitude).runFold(0L)(_ max _)

  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day18/day18-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day18/day18-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
