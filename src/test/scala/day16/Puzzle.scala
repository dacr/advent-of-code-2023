package day16

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

import scala.annotation.tailrec

enum Direction(val dx: Int, val dy: Int) {
  case Right extends Direction(1, 0)
  case Left  extends Direction(-1, 0)
  case North extends Direction(0, -1)
  case South extends Direction(0, 1)
}
import Direction.*

case class Coord(x: Int, y: Int) {
  def move(direction: Direction): Coord =
    Coord(x + direction.dx, y + direction.dy)
}

def continue(moving: Direction): List[Direction] = {
  moving :: Nil
}

def verticalSplit(moving: Direction): List[Direction] = {
  moving match {
    case Right | Left => North :: South :: Nil
    case other        => other :: Nil
  }
}

def horizontalSplit(moving: Direction): List[Direction] = {
  moving match {
    case North | South => Right :: Left :: Nil
    case other         => other :: Nil
  }
}

def mirror90(moving: Direction): List[Direction] = {
  moving match {
    case Right => North :: Nil
    case Left  => South :: Nil
    case North => Right :: Nil
    case South => Left :: Nil
  }
}

def mirror270(moving: Direction): List[Direction] = {
  moving match {
    case Right => South :: Nil
    case Left  => North :: Nil
    case North => Left :: Nil
    case South => Right :: Nil
  }
}

enum Cell(val code: Char, val behavior: Direction => List[Direction]) {
  case EmptySpace         extends Cell('.', continue)
  case VerticalSplitter   extends Cell('|', verticalSplit)
  case HorizontalSplitter extends Cell('-', horizontalSplit)
  case Mirror90           extends Cell('/', mirror90)
  case Mirror270          extends Cell('\\', mirror270)
}

case class Grid(coords: Map[Coord, Cell])

// ------------------------------------------------------------------------------
def parse(input: List[String]) = {
  val grid = for {
    (row, y)  <- input.zipWithIndex
    (code, x) <- row.zipWithIndex
    cellOption = Cell.values.find(_.code == code)
    if cellOption.isDefined
  } yield Coord(x, y) -> cellOption.get

  Grid(grid.toMap)
}

// ------------------------------------------------------------------------------

def energized(from: Coord, direction: Direction, grid: Grid): Int = {
  @tailrec
  def worker(beams: List[(Coord, Direction)], visited: Set[(Coord, Direction)]): Set[(Coord, Direction)] = {
    import grid.coords
    beams match {
      case Nil                                                   => visited
      case (coord, direction) :: tail if !coords.contains(coord) => worker(tail, visited)
      case head :: tail if visited.contains(head)                => worker(tail, visited)
      case (coord, direction) :: tail                            =>
        val nextBeams = coords(coord).behavior(direction).map(newDirection => coord.move(newDirection) -> newDirection)
        worker(nextBeams ::: tail, visited + (coord -> direction))
    }
  }
  worker((from, direction) :: Nil, Set.empty).collect { case (coord, direction) => coord }.size
}

def resolveStar1(input: List[String]): Int = {
  val grid = parse(input)
  energized(Coord(0, 0), Right, grid)
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val grid = parse(input)
  val maxX = grid.coords.keys.maxBy(_.x).x
  val maxY = grid.coords.keys.maxBy(_.y).y

  val hresults =
    (0.to(maxY).flatMap(y => energized(Coord(0, y), Right, grid) :: energized(Coord(maxX, y), Left, grid) :: Nil))

  val vresults =
    (0.to(maxX).flatMap(x => energized(Coord(x, 0), South, grid) :: energized(Coord(x, maxY), North, grid) :: Nil))

  max(hresults.max, vresults.max)
}

// ------------------------------------------------------------------------------

object Puzzle16Test extends ZIOSpecDefault {
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
        exampleResult == 46,
        puzzleResult == 7199
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 51,
        puzzleResult == 7438
      )
    }
  ) @@ timed @@ sequential
}
