package day14

import zio.*
import zio.test.*
import zio.test.TestAspect.*

case class Coord(x: Int, y: Int)

case class Platform(
  rollingRocks: Set[Coord],
  cubicRocks: Set[Coord],
  maxX: Int,
  maxY: Int
)

// ------------------------------------------------------------------------------
def parse(input: List[String]) = {
  val cells        = for {
    (row, y)  <- input.zipWithIndex
    (cell, x) <- row.zipWithIndex
    if cell != '.'
  } yield Coord(x, y) -> cell
  val maxX         = cells.maxBy((coord, _) => coord.x)._1.x
  val maxY         = cells.maxBy((coord, _) => coord.y)._1.y
  val cellsByType  = cells.groupMap((coord, cell) => cell)((coord, cell) => coord)
  val rollingRocks = cellsByType('O').toSet
  val cubicRocks   = cellsByType('#').toSet
  Platform(rollingRocks, cubicRocks, maxX, maxY)
}

// ------------------------------------------------------------------------------
def slideNorth(rollingRocks: Set[Coord], platform: Platform): Set[Coord] = {
  val rollingRocksByX     = rollingRocks.groupBy(_.x)
  var updatedRollingRocks = rollingRocks
  0.to(platform.maxX).map { x =>
    0.to(platform.maxY).map { y =>
      val coord = Coord(x, y)

    }
  }
  ???
}

def resolveStar1(input: List[String]): Int = {
  val platform          = parse(input)
  val movedRollingRocks = slideNorth(platform.rollingRocks, platform)
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
