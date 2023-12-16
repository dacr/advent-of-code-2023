package day14

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
def parse(input: List[String]) = {
  val cells = for {
    (row, y)  <- input.zipWithIndex
    (cell, x) <- row.zipWithIndex
    if cell != '.'
  } yield (x, y) -> cell

  val (rounded, cubes) = cells.partition { case (_, cell) => cell == 'O' }
  (rounded, cubes)
}

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int = {
  val (rounded, cubes) = parse(input)
  ???
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle14Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 136,
        puzzleResult == 0
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 0,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
