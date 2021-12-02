package freskog.aoc.day2

import zio._
import zio.test._

import freskog.aoc.utils._
import freskog.aoc.day2.Day2Solution

object Day2SolutionSpec extends DefaultRunnableSpec {

  val spec =
    suite("Day2")(
      suite("Part1")(
        test("150 is the answer from the test input")(
          Day2Solution
            .part1("day2/day2-test-input-part-1.txt")
            .map(result => assertTrue(result == 150))
        )
      ),
      suite("Part2")(
        test("900 is the answer from the test input")(
          Day2Solution
            .part2("day2/day2-test-input-part-1.txt")
            .map(result => assertTrue(result == 900))
        )
      )
    )

}
