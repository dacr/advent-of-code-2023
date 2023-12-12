package day11

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

import scala.annotation.tailrec

case class Coord(x: Long, y: Long) {
  def around = List(
    Coord(x - 1, y),
    Coord(x + 1, y),
    Coord(x, y - 1),
    Coord(x, y + 1)
  )
}

case class Universe(
  galaxies: List[Coord],
  maxX: Long,
  maxY: Long,
  emptyX: List[Long],
  emptyY: List[Long]
) {
  val galaxiesSet = galaxies.toSet

  def hasGalaxyAt(coord: Coord): Boolean = {
    galaxiesSet.contains(coord)
  }

  def inRange(coord: Coord): Boolean = {
    coord.x >= 0 && coord.y >= 0 && coord.x <= maxX && coord.y <= maxY
  }

}

// ------------------------------------------------------------------------------
def parse(input: List[String]) = {
  val galaxies = for {
    (line, y) <- input.zipWithIndex
    (char, x) <- line.zipWithIndex
    if char == '#'
  } yield Coord(x, y)
  val allX     = galaxies.map(_.x).toSet
  val allY     = galaxies.map(_.y).toSet
  val maxX     = allX.max
  val maxY     = allY.max
  val emptyX   = 0L.to(maxX).filterNot(allX.contains).toList
  val emptyY   = 0L.to(maxY).filterNot(allY.contains).toList
  Universe(
    galaxies = galaxies,
    maxX = maxX,
    maxY = maxY,
    emptyX = emptyX,
    emptyY = emptyY
  )
}

// ------------------------------------------------------------------------------

// TODO - TO generalize for future reuses
//type Path = List[Coord]
//case class Work(path: Path)

//def bfs(from: Coord, around: Coord => List[Coord], goalReached: Coord => Boolean): List[Path] = {
//  @tailrec
//  def worker(toVisit: List[Work], visited: Set[Coord], solutions: List[Path]): List[Path] = {
//    toVisit match {
//      case Nil => solutions
//
//      case Work(path) :: remainWork if visited.contains(path.head) => worker(remainWork, visited, solutions)
//
//      case Work(path) :: remainWork if goalReached(path.head) => worker(remainWork, visited + path.head, path :: solutions)
//
//      case Work(path @ coord :: _) :: remainWork =>
//        val nextVisited = visited + coord
//        val nextCoords  = around(coord).filterNot(nextVisited.contains)
//        val nextToVisit = remainWork :++ nextCoords.map(nextCoord => Work(nextCoord :: path))
//        worker(nextToVisit, nextVisited, solutions)
//    }
//  }
//
//  worker(Work(from :: Nil) :: Nil, Set(), Nil)
//}
//
//def orderCoords(coords: (Coord, Coord)): (Coord, Coord) = {
//  coords match {
//    case (a, b) if a.y < b.y               => (a, b)
//    case (a, b) if a.y == b.y && a.x < b.x => (a, b)
//    case (a, b)                            => (b, a)
//  }
//}

//def resolveStar1(input: List[String]): Int = {
//  val universe = parse(input)
//  val paths    = universe.galaxies.flatMap { fromGalaxy =>
//    val around      = (coord: Coord) => coord.around.filter(subCoord => universe.inRange(subCoord))
//    val goalReached = (coord: Coord) => universe.hasGalaxyAt(coord)
//    val solutions   = bfs(fromGalaxy, around, goalReached)
//    solutions.map(solution => (fromGalaxy, solution.head) -> solution)
//  }
//
//  val minPaths = paths.groupMapReduce { case ((from, to), path) => orderCoords((from, to)) } { case ((from, to), path) => path }((path1, path2) => if (path1.size < path2.size) path1 else path2)
//
//  minPaths.values.map(_.size).sum
//}

def manhattanDistance(from: Coord, to: Coord): Long = {
  abs(to.x - from.x) + abs(to.y - from.y)
}

def dilate(universe: Universe, factor: Long)(coord: Coord): Coord = {
  val expandedX = coord.x + universe.emptyX.count(_ < coord.x) * (factor - 1)
  val expandedY = coord.y + universe.emptyY.count(_ < coord.y) * (factor - 1)
  Coord(expandedX, expandedY)
}

def resolveStar1(input: List[String]): Long = {
  val universe = parse(input)
  universe.galaxies
    .map(dilate(universe, 2))
    .combinations(2)
    .collect { case from :: to :: Nil => manhattanDistance (from, to) }
    .sum
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String], factor:Long): Long =
  val universe = parse(input)
  universe.galaxies
    .map(dilate(universe, factor))
    .combinations(2)
    .collect { case from :: to :: Nil => manhattanDistance(from, to) }
    .sum

// ------------------------------------------------------------------------------

object Puzzle11Test extends ZIOSpecDefault {
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
        exampleResult == 374,
        puzzleResult == 9370588
      )
    },
    test("star#2") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult10 = resolveStar2(exampleInput, 10)
        exampleResult100 = resolveStar2(exampleInput, 100)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput, 1_000_000L)
      } yield assertTrue(
        exampleResult10 == 1030,
        exampleResult100 == 8410,
        puzzleResult == 746207878188L
      )
    }
  ) @@ timed @@ sequential
}
