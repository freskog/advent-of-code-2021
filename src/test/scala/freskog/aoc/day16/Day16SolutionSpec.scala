package freskog.aoc.day16

import freskog.aoc.day16.Day16Solution._
import zio.test.DefaultRunnableSpec
import zio.test._

object Day16SolutionSpec extends DefaultRunnableSpec {

  val spec =
    suite("Day16")(
      suite("Part1")(
        test("Convert input hex to binary") {
          val actual   = PacketParser.convertFromHexToBinary("D2FE28")
          val expected = "110100101111111000101000"
          assertTrue(actual == expected)
        },
        suite("Parse Literal/Operator")(
          test("Parse literal number") {
            val packet = Day16Solution.PacketParser
              .packetFrom("110100101111111000101000")
            assertTrue(packet == Packet(6, PktType.Lit, 21, PacketData.Literal(BigDecimal(2021))))
          },
          test("Parse operator with length specified in bits") {
            val packet = Day16Solution.PacketParser
              .packetFrom("00111000000000000110111101000101001010010001001000000000")
            assertTrue(
              packet ==
                Packet(
                  1,
                  PktType.Less,
                  49,
                  PacketData.Operator(
                    List(
                      Packet(6, PktType.Lit, 11, PacketData.Literal(BigDecimal(10))),
                      Packet(2, PktType.Lit, 16, PacketData.Literal(BigDecimal(20)))
                    )
                  )
                )
            )
          },
          test("Parse operator with length specified by number of sub-packets") {
            val packet = Day16Solution.PacketParser
              .packetFrom("11101110000000001101010000001100100000100011000001100000")
            assertTrue(
              packet ==
                Packet(
                  7,
                  PktType.Max,
                  51,
                  PacketData.Operator(
                    List(
                      Packet(2, PktType.Lit, 11, PacketData.Literal(BigDecimal(1))),
                      Packet(4, PktType.Lit, 11, PacketData.Literal(BigDecimal(2))),
                      Packet(1, PktType.Lit, 11, PacketData.Literal(BigDecimal(3)))
                    )
                  )
                )
            )
          }
        ),
        suite("Test summing of versions in packet")(
          test("versions in 8A004A801A8002F478 sum to 16") {
            assertTrue(buildPacket("8A004A801A8002F478").sumVersions == 16)
          },
          test("versions in 620080001611562C8802118E34 sum to 12") {
            assertTrue(buildPacket("620080001611562C8802118E34").sumVersions == 12)
          },
          test("versions in C0015000016115A2E0802F182340 sum to 23") {
            assertTrue(buildPacket("C0015000016115A2E0802F182340").sumVersions == 23)
          },
          test("versions in A0016C880162017C3686B18A3D4780 sum to 31") {
            assertTrue(buildPacket("A0016C880162017C3686B18A3D4780").sumVersions == 31)
          }
        )
      ),
      suite("Part2")(
        suite("evaluate packets")(
          test("C200B40A82 evaluates to 3") {
            assertTrue(buildPacket("C200B40A82").eval.toInt == 3)
          },
          test("04005AC33890 evaluates to 54") {
            assertTrue(buildPacket("04005AC33890").eval.toInt == 54)
          },
          test("880086C3E88112 evaluates to 7") {
            assertTrue(buildPacket("880086C3E88112").eval.toInt == 7)
          },
          test("CE00C43D881120 evaluates to 9") {
            assertTrue(buildPacket("CE00C43D881120").eval.toInt == 9)
          },
          test("D8005AC2A8F0 evaluates to 1") {
            assertTrue(buildPacket("D8005AC2A8F0").eval.toInt == 1)
          },
          test("F600BC2D8F evaluates to 0") {
            assertTrue(buildPacket("F600BC2D8F").eval.toInt == 0)
          },
          test("9C005AC2F8F0 evaluates to 0") {
            assertTrue(buildPacket("9C005AC2F8F0").eval.toInt == 0)
          },
          test("9C0141080250320F1802104A08 evaluates to 1") {
            assertTrue(buildPacket("9C0141080250320F1802104A08").eval.toInt == 1)
          },
        )
      )
    )
}
