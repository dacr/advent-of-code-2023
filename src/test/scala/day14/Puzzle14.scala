package day14

import zio.*
import zio.test.*
import zio.test.TestAspect.*

opaque type Coord = Int

extension (c: Coord) {
  def x      = c % Coord.width
  def y      = c / Coord.width
  def right  = c + 1
  def left   = c - 1
  def north  = c - Coord.width
  def south  = c + Coord.width
  def around = List(c.south, c.right, c.north, c.left)
}
object Coord {
  val width                        = 10000
  def apply(x: Int, y: Int): Coord = y * Coord.width + x
}

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
  var updatedRollingRocks = rollingRocks
  var currentX            = 0
  while (currentX <= platform.maxX) {
    var currentY = 0
    while (currentY <= platform.maxY) {
      val current        = Coord(currentX, currentY)
      def currentIsEmpty = !updatedRollingRocks.contains(current) && !platform.cubicRocks.contains(current)
      var other          = current.south
      while (currentIsEmpty && other.y <= platform.maxY && !platform.cubicRocks.contains(other)) {
        if (updatedRollingRocks.contains(other)) {
          updatedRollingRocks = updatedRollingRocks - other + current
        }
        other = other.south
      }
      currentY = currentY + 1
    }
    currentX = currentX + 1
  }
  updatedRollingRocks
}

def valueNorth(rollingRocks: Set[Coord], platform: Platform): Int = {
  0.to(platform.maxY)
    .map { y =>
      val count = 0.to(platform.maxX).count(x => rollingRocks.contains(Coord(x, y)))
      count * (1 + platform.maxY - y)
    }
    .sum
}

def show(rollingRocks: Set[Coord], platform: Platform): Unit = {
  0.to(platform.maxY).foreach { y =>
    0.to(platform.maxX).foreach { x =>
      val coord = Coord(x, y)
      if (platform.cubicRocks.contains(coord)) print("#")
      else if (rollingRocks.contains(coord)) print("O")
      else print(".")
    }
    println()
  }
}

def resolveStar1(input: List[String]): Int = {
  val platform          = parse(input)
  val movedRollingRocks = slideNorth(platform.rollingRocks, platform)
  //show(movedRollingRocks, platform)
  valueNorth(movedRollingRocks, platform)
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
        puzzleResult == 105982
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 64,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
