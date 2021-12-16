package freskog.aoc.day16

import zio._

import freskog.aoc.utils._

import fastparse._, NoWhitespace._

object Day16Solution extends ZIOAppDefault {

  case class Packet(version: Int, typeId: PktType, bits: Int, data: PacketData) {

    def eval: BigDecimal =
      typeId match {
        case PktType.Sum     => data.eval.sum
        case PktType.Mul     => data.eval.product
        case PktType.Min     => data.eval.min
        case PktType.Max     => data.eval.max
        case PktType.Lit     => data.eval.head
        case PktType.Greater => data.eval match { case n1 :: n2 :: Nil => if(n1 > n2) 1 else 0 }
        case PktType.Less    => data.eval match { case n1 :: n2 :: Nil => if(n1 < n2) 1 else 0 }
        case PktType.EqualTo => data.eval match { case n1 :: n2 :: Nil => if(n1 == n2) 1 else 0}
      }

    def sumVersions: Int =
      version + data.sumVersions
  }

  sealed trait PktType
  object PktType {
    case object Sum     extends PktType
    case object Mul     extends PktType
    case object Min     extends PktType
    case object Max     extends PktType
    case object Lit     extends PktType
    case object Greater extends PktType
    case object Less    extends PktType
    case object EqualTo extends PktType

    def from(int: Int): PktType =
      int match {
        case 0 => Sum
        case 1 => Mul
        case 2 => Min
        case 3 => Max
        case 4 => Lit
        case 5 => Greater
        case 6 => Less
        case 7 => EqualTo
      }

  }

  sealed trait PacketData { self =>
    def sumVersions: Int       =
      self match {
        case PacketData.Literal(_)           => 0
        case PacketData.Operator(subPackets) => subPackets.map(_.sumVersions).sum
      }
    def eval: List[BigDecimal] =
      self match {
        case PacketData.Literal(n)           => List(n)
        case PacketData.Operator(subPackets) => subPackets.map(_.eval)
      }
  }
  object PacketData       {
    final case class Literal(n: BigDecimal)             extends PacketData
    final case class Operator(subPackets: List[Packet]) extends PacketData
  }

  object PacketParser {

    def binaryStrToNumber(binStr: String): BigDecimal = {
      def build(remaining: String, pow: BigDecimal, acc: BigDecimal): BigDecimal =
        if (remaining.isEmpty) acc else build(remaining.tail, pow * 2, if (remaining.head == '1') acc + pow else acc)
      build(binStr.reverse, BigDecimal(1), BigDecimal(0))
    }

    def binaryDigit[_: P]: P[String] = P(("1" | "0").!)

    def binaryBig[_: P](len: Int): P[BigDecimal] = binaryDigit.rep(exactly = len).map(numbers => binaryStrToNumber(numbers.mkString))

    def binaryInt[_: P](len: Int): P[Int] = binaryBig(len).map(_.toInt)

    def version[_: P]: P[Int] = binaryInt(3)

    def typeId[_: P]: P[Int] = binaryInt(3)

    def literalStr[_: P]: P[(Int, String)] = binaryDigit.flatMap {
      case "0" => binaryDigit.rep(exactly = 4).!.map((5, _))
      case "1" =>
        binaryDigit.rep(exactly = 4).!.flatMap { case grp =>
          literalStr.map { case (accLen, accGrp) => (5 + accLen, grp ++ accGrp) }
        }
    }

    def operandsBySubpackets[_: P](numberOfSubPackets: Int): P[PacketData.Operator] =
      packet.rep(exactly = numberOfSubPackets).map(_.toList).map(PacketData.Operator)

    def operandsByTotalBits[_: P](numberOfBits: Int): P[PacketData.Operator] =
      if (numberOfBits == 0) Pass(PacketData.Operator(Nil))
      else packet.flatMap(pkt => operandsByTotalBits(numberOfBits - pkt.bits).map(op => PacketData.Operator(pkt :: op.subPackets)))

    def literalNumber[_: P]: P[(Int, PacketData.Literal)] =
      literalStr.map { case (lenInBits, str) =>
        (lenInBits, PacketData.Literal(binaryStrToNumber(str)))
      }

    def genericOperator[_: P]: P[(Int, PacketData.Operator)] = binaryDigit.flatMap {
      case "0" => binaryInt(15).flatMap(bits => operandsByTotalBits(bits).map((bits + 16, _)))
      case "1" =>
        binaryInt(11).flatMap { pkts =>
          operandsBySubpackets(pkts).map { case PacketData.Operator(subPackets) =>
            val bits = subPackets.map(_.bits).sum + 12
            (bits, PacketData.Operator(subPackets))
          }
        }
    }

    def packetData[_: P](id: Int): P[(Int, PacketData)] = id match {
      case 4 => literalNumber
      case _ => genericOperator
    }

    def packet[_: P]: P[Packet] =
      for {
        v           <- version
        id          <- typeId
        bitsAndData <- packetData(id)
        (bits, data) = bitsAndData
      } yield Packet(v, PktType.from(id), bits + 6, data)

    def convertFromHexToBinary(hexStr: String): String =
      hexStr.flatMap {
        case '0' => List("0000")
        case '1' => List("0001")
        case '2' => List("0010")
        case '3' => List("0011")
        case '4' => List("0100")
        case '5' => List("0101")
        case '6' => List("0110")
        case '7' => List("0111")
        case '8' => List("1000")
        case '9' => List("1001")
        case 'A' => List("1010")
        case 'B' => List("1011")
        case 'C' => List("1100")
        case 'D' => List("1101")
        case 'E' => List("1110")
        case 'F' => List("1111")
      }.mkString

    def packetFrom(input: String): Packet = {
      val Parsed.Success(data, _) = parse(input, packet(_))
      data
    }
  }

  def buildPacket(inputAsHex:String):Packet =
    PacketParser.packetFrom(PacketParser.convertFromHexToBinary(inputAsHex))

  def part1(inputPath: String) =
    readAllAsString(inputPath).map(buildPacket(_).sumVersions)

  def part2(inputPath: String) =
    readAllAsString(inputPath).map(buildPacket(_).eval)


  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
    part1("day16/day16-input-part-1.txt").flatMap(answer => Console.printLine(s"Part1: $answer")) *>
      part2("day16/day16-input-part-1.txt").flatMap(answer => Console.printLine(s"Part2: $answer"))
}
