package freskog.aoc.day3

import zio.test.DefaultRunnableSpec

import zio.test._


object Day3SolutionSpec extends DefaultRunnableSpec {

    val spec =
    suite("Day3")(
      suite("Part1")(
        test("198 is the answer from the test input")(
          Day3Solution
            .part1("day3/day3-test-input-part-1.txt")
            .map(result => assertTrue(result == 198))
        )
      ),
       suite("Part2")(
        test("230 is the answer from the test input")(
          Day3Solution
            .part2("day3/day3-test-input-part-1.txt")
            .map(result => assertTrue(result == 230))
        )
      )
    )
}