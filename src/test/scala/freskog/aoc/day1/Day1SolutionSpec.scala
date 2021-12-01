package freskog.aoc.day1

import freskog.aoc.utils.readAsOneLongPerLine
import zio.test._

object Day1SolutionSpec extends DefaultRunnableSpec {

  val testInput = readAsOneLongPerLine("day1/day1-test-input-part-1.txt")

  val spec =
    suite("Day1")(
      suite("Part1")(
        test("7 increases in the test input")(
          Day1Solution
            .part1(testInput)
            .runCount
            .map(result => assertTrue(result == 7L))
        )
      ),
      suite("Part2")(
        test("5 increases in the test input")(
          Day1Solution
            .part2(testInput)
            .runCount
            .map(result => assertTrue(result == 5L))
        )
      )
    )

}
