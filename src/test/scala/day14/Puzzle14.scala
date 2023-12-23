package day14

import zio.*
import zio.test.*
import zio.test.TestAspect.*

opaque type Coord = Int

extension (c: Coord) {
  def x      = c % Coord.width
  def y      = c / Coord.width
  def east   = c + 1
  def west   = c - 1
  def north  = c - Coord.width
  def south  = c + Coord.width
  def around = List(c.south, c.east, c.north, c.west)
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
def slideNorth(platform: Platform)(rollingRocks: Set[Coord]): Set[Coord] = {
  var updatedRollingRocks = rollingRocks
  var currentX            = 0
  while (currentX <= platform.maxX) {
    var currentY = 0
    while (currentY <= platform.maxY) {
      val current        = Coord(currentX, currentY)
      def currentIsEmpty = !updatedRollingRocks.contains(current) && !platform.cubicRocks.contains(current)
      var other          = current
      while (currentIsEmpty && other.y < platform.maxY && !platform.cubicRocks.contains(other)) {
        other = other.south
        if (updatedRollingRocks.contains(other)) {
          updatedRollingRocks = updatedRollingRocks - other + current
        }
      }
      currentY = currentY + 1
    }
    currentX = currentX + 1
  }
  updatedRollingRocks
}

def slideSouth(platform: Platform)(rollingRocks: Set[Coord]): Set[Coord] = {
  var updatedRollingRocks = rollingRocks
  var currentX            = 0
  while (currentX <= platform.maxX) {
    var currentY = platform.maxY
    while (currentY >= 0) {
      val current        = Coord(currentX, currentY)
      def currentIsEmpty = !updatedRollingRocks.contains(current) && !platform.cubicRocks.contains(current)
      var other          = current
      while (currentIsEmpty && other.y > 0 && !platform.cubicRocks.contains(other)) {
        other = other.north
        if (updatedRollingRocks.contains(other)) {
          updatedRollingRocks = updatedRollingRocks - other + current
        }
      }
      currentY = currentY - 1
    }
    currentX = currentX + 1
  }
  updatedRollingRocks
}

def slideWest(platform: Platform)(rollingRocks: Set[Coord]): Set[Coord] = {
  var updatedRollingRocks = rollingRocks
  var currentY            = 0
  while (currentY <= platform.maxY) {
    var currentX = 0
    while (currentX <= platform.maxX) {
      val current        = Coord(currentX, currentY)
      def currentIsEmpty = !updatedRollingRocks.contains(current) && !platform.cubicRocks.contains(current)
      var other          = current
      while (currentIsEmpty && other.x < platform.maxX && !platform.cubicRocks.contains(other)) {
        other = other.east
        if (updatedRollingRocks.contains(other)) {
          updatedRollingRocks = updatedRollingRocks - other + current
        }
      }
      currentX = currentX + 1
    }
    currentY = currentY + 1
  }

  updatedRollingRocks
}

def slideEast(platform: Platform)(rollingRocks: Set[Coord]): Set[Coord] = {
  var updatedRollingRocks = rollingRocks
  var currentY            = 0
  while (currentY <= platform.maxY) {
    var currentX = platform.maxX
    while (currentX >= 0) {
      val current        = Coord(currentX, currentY)
      def currentIsEmpty = !updatedRollingRocks.contains(current) && !platform.cubicRocks.contains(current)
      var other          = current
      while (currentIsEmpty && other.x > 0 && !platform.cubicRocks.contains(other)) {
        other = other.west
        if (updatedRollingRocks.contains(other)) {
          updatedRollingRocks = updatedRollingRocks - other + current
        }
      }
      currentX = currentX - 1
    }
    currentY = currentY + 1
  }
  //  show(platform)(updatedRollingRocks)
  // println(valueNorth(platform)(updatedRollingRocks))
  updatedRollingRocks
}

def valueNorth(platform: Platform)(rollingRocks: Set[Coord]): Int = {
  0.to(platform.maxY)
    .map { y =>
      val count = 0.to(platform.maxX).count(x => rollingRocks.contains(Coord(x, y)))
      count * (1 + platform.maxY - y)
    }
    .sum
}

def show(platform: Platform)(rollingRocks: Set[Coord]): Unit = {
  println("---------------------------------------------------")
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
  val movedRollingRocks = slideNorth(platform)(platform.rollingRocks)
  // show(platform)(movedRollingRocks)
  valueNorth(platform)(movedRollingRocks)
}

// ------------------------------------------------------------------------------

case class Cycle(
  start: Int,
  size: Int
)

def findCycle(samples: Iterable[Int], minSize: Int, maxSizeOption: Option[Int]): Option[Cycle] = {
  val length = samples.size
  val search = samples.zipWithIndex.flatMap { case (sample, start) =>
    val left    = samples.drop(start)
    val maxSize = maxSizeOption.getOrElse(length - minSize)
    (minSize.to(maxSize)).flatMap { case size =>
      val right = samples.drop(start + size)
      if (left.zip(right).forall(_ == _)) Some(Cycle(start, size))
      else None
    }
  }
  search.minByOption(_.size)
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val platform = parse(input)

  def cyclingLambdas = LazyList
    .continually(
      List(slideNorth(platform), slideWest(platform), slideSouth(platform), slideEast(platform))
    )
    .flatten
    // .take(1_000_000_000)
    .take(4 * 200)

  val (movedRollingRocks, samples) = cyclingLambdas.foldLeft((platform.rollingRocks, Vector.empty[Int])) { case ((coords, samples), op) =>
    val moved  = op(coords)
    val sample = valueNorth(platform)(moved)
    //show(platform)(moved)
    //println(sample)
    (moved, samples :+ sample)
  }

  val foundCycle = findCycle(samples, 5, None)
  foundCycle match {
    case None        => ???
    case Some(cycle) =>
      val target = 1_000_000_000
      samples.lift(cycle.start + (target - cycle.start) % cycle.size).get
  }
}

// ------------------------------------------------------------------------------

object Puzzle14Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("find cycles") {
      assertTrue(
        findCycle(List(2, 3, 4, 2, 3, 4, 2, 3, 4), 2, None).contains(Cycle(0, 3)),
        findCycle(List(1, 2, 3, 4, 2, 3, 4, 2, 3, 4), 2, None).contains(Cycle(1, 3)),
        findCycle(List(0, 1, 2, 3, 4, 2, 3, 4, 2, 3, 4), 2, None).contains(Cycle(2, 3))
      )
    },
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
        puzzleResult > 85157,
        puzzleResult < 93068,
        puzzleResult < 93042,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
