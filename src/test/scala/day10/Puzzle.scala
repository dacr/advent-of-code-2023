package day10

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.io.AnsiColor.{RED, BLUE, GREEN, YELLOW, CYAN, MAGENTA, RESET}

// ------------------------------------------------------------------------------
case class Coord(x: Int, y: Int) {
  def west  = Coord(x - 1, y)
  def east  = Coord(x + 1, y)
  def north = Coord(x, y - 1)
  def south = Coord(x, y + 1)
}

case class Pipe(coord: Coord, code: Char, connectCoords: List[Coord]) {
  def isStart                     = code == '⭐'
  val coords                      = coord :: connectCoords
  def coordsFrom(endpoint: Coord) = connectCoords.find(_ != endpoint) match {
    case Some(otherCoord) => otherCoord :: coord :: Nil
    case None             => Nil
  }
}

def buildPipe(coord: Coord, code: Char): Option[Pipe] = {
  code match {
    case '|' => Some(Pipe(coord, '┃', List(coord.north, coord.south)))
    case '-' => Some(Pipe(coord, '━', List(coord.east, coord.west)))
    case 'L' => Some(Pipe(coord, '┗', List(coord.north, coord.east)))
    case 'J' => Some(Pipe(coord, '┛', List(coord.north, coord.west)))
    case '7' => Some(Pipe(coord, '┓', List(coord.south, coord.west)))
    case 'F' => Some(Pipe(coord, '┏', List(coord.south, coord.east)))
    case '.' => Some(Pipe(coord, ' ', List.empty))
    case 'S' => Some(Pipe(coord, '⭐', List.empty))
  }
}

def parse(input: List[String]) = {
  val pipes = for {
    (line, y) <- input.zipWithIndex
    (char, x) <- line.zipWithIndex
  } yield buildPipe(Coord(x, y), char)
  pipes.flatten
}

// ------------------------------------------------------------------------------

case class Work(path: List[Coord], depth: Int)

def show(toVisit: List[Work], pipes: Map[Coord, Pipe]): Unit = {
  val maxX = pipes.keys.map(_.x).max
  val maxY = pipes.keys.map(_.y).max
  0.to(maxY).foreach { y =>
    0.to(maxX).foreach { x =>
      val coord = Coord(x, y)
      val ch    = pipes.get(coord).map(_.code).getOrElse(".")
      toVisit.map(_.path.contains(coord)).zip(List(RED, BLUE, GREEN, YELLOW, CYAN, MAGENTA)).find { case (result, color) => result } match {
        case Some((_, color)) => print(s"$color$ch$RESET")
        case None             => print(ch)
      }
    }
    println()
  }
  println()
}

def farthest(from: Coord, links: Map[Coord, List[Pipe]], pipes: Map[Coord, Pipe]): Int = {
  @tailrec
  def worker(toVisit: List[Work], visited: Set[Coord]): Int = {
    toVisit match {
      case Work(head :: path, depth) :: Nil if visited.size > 0 => path.size

      case Work(head :: _, depth) :: remainWork if visited.contains(head) => worker(remainWork, visited)

      case Work(path, depth) :: remainWork =>
        //show(toVisit, pipes)

        val coord       = path.head
        val nextVisited = visited ++ path
        val nextCoords  = pipes(coord).connectCoords.filterNot(nextVisited.contains)
        if (nextCoords.isEmpty) path.size
        else {
          val nextToVisit = remainWork :+ Work(nextCoords ++ path, depth + 1) // TODO depth compute is false
          worker(nextToVisit, nextVisited)
        }
    }
  }

  val connectedToStart = links(from).map(_.coordsFrom(from))
  worker(connectedToStart.map(path => Work(path, 0)), Set())
}

def resolveStar1(input: List[String]): Int =
  val pipes = parse(input)
  val start = pipes.find(_.isStart).get // we always have one
  val links =
    pipes
      .flatMap(pipe => pipe.connectCoords.map(coord => coord -> pipe))
      .groupMap((coord, pipes) => coord)((coord, pipes) => pipes)
  farthest(start.coord, links, pipes.groupMapReduce(_.coord)(p => p)((a, b) => a))

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle10Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar1(exampleInput1)
        exampleInput2 <- fileLines(Path(s"data/$day/example-2.txt"))
        exampleResult2 = resolveStar1(exampleInput2)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 4,
        exampleResult2 == 8,
        puzzleResult < 8293,
        puzzleResult > 1206,
        puzzleResult == 6979
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
