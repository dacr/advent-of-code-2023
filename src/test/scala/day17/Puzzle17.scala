package day17

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.math.*
import scala.io.AnsiColor.{RED_B, RESET}

// ------------------------------------------------------------------------------

case class Coord(x: Int, y: Int) {
  def right  = Coord(x + 1, y)
  def left   = Coord(x - 1, y)
  def north  = Coord(x, y - 1)
  def south  = Coord(x, y + 1)
  def around = List(right, south, north, left)
}

case class Area(cells: Map[Coord, Int], maxX: Int, maxY: Int)

def parse(input: List[String]) = {
  val maxX  = input.head.size - 1
  val maxY  = input.size - 1
  val cells = for {
    (row, y)  <- input.zipWithIndex
    (cell, x) <- row.zipWithIndex
  } yield Coord(x, y) -> cell.toString.toInt
  Area(cells.toMap, maxX, maxY)
}

// ------------------------------------------------------------------------------
type Path = List[Coord]
case class Solution(path: Path, weights: List[Int])
case class Work(path: Path, visited: Set[Coord], weights: List[Int]) {
  def coord  = path.head
  def weight = weights.headOption.getOrElse(0)
}

def dump(area: Area)(work: Work): Unit = {
  val pathCoords = work.path.toSet
  if (pathCoords.size != work.path.size) ???
  0.to(area.maxY).foreach { y =>
    0.to(area.maxX).foreach { x =>
      val coord = Coord(x, y)
      val value = area.cells(coord)
      if (pathCoords.contains(coord)) print(s"$RED_B$value$RESET")
      else print(value)
    }
    println()
  }
  println(pathCoords.toList.init.map(c => area.cells(c)).sum)
  println(work.weights.mkString("-"))
}

def walk(
  from: Coord,
  around: Work => List[Coord],
  goalReached: Work => Boolean,
  coord2weight: Coord => Int,
  dump: Work => Unit
): List[Solution] = {
  @tailrec
  def worker(
    toVisit: List[Work],
    solutions: List[Solution],
    bestWeightAt: Map[Coord, Int],
    bestPathWeight: Option[Int]
  ): List[Solution] = {
    toVisit match {
      case Nil =>
        solutions

      case work :: remainWork if bestPathWeight.exists(best => work.weight > best) =>
        worker(remainWork, solutions, bestWeightAt, bestPathWeight)

      case work :: remainWork if bestWeightAt.get(work.coord).exists(best => work.weight > best) =>
        worker(remainWork, solutions, bestWeightAt, bestPathWeight)

      case work :: remainWork if goalReached(work) =>
        val newSolutions      = Solution(work.path, work.weights) :: solutions
        dump(work)
        val newBestWeightAt   = work.path.zip(work.weights).foldLeft(bestWeightAt) { case (bests, (coord, weight)) =>
          bests + (coord -> bestWeightAt.get(coord).map(prevWeight => min(prevWeight, weight)).getOrElse(weight))
        }
        val newBestPathWeight = bestPathWeight.map(best => min(best, work.weight)).orElse(Some(work.weight))
        worker(remainWork, newSolutions, newBestWeightAt, newBestPathWeight)

      case work :: remainWork =>
        val nextCoords  = around(work).filterNot(work.visited.contains)
        val nextToVisit = nextCoords.map { nextCoord =>
          Work(nextCoord :: work.path, work.visited + nextCoord, nextWeight(nextCoord, work, coord2weight))
        }
        worker(nextToVisit ::: remainWork, solutions, bestWeightAt, bestPathWeight)
    }
  }
  worker(Work(List(from), Set(from), Nil) :: Nil, Nil, Map.empty, None)
}

def nextWeight(nextCoord: Coord, work: Work, coord2weight: Coord => Int): List[Int] = {
  work.path match {
    case Nil => Nil
    case _   => (work.weight + coord2weight(nextCoord)) :: work.weights
  }
}

// ------------------------------------------------------------------------------

def checkValid(c1: Coord, work: Work): Boolean = {
  work.path match {
    case c2 :: c3 :: c4 :: c5 :: _ if c1.x == c2.x && c1.x == c3.x && c1.x == c4.x && c1.x == c5.x => false
    case c2 :: c3 :: c4 :: c5 :: _ if c1.y == c2.y && c1.y == c3.y && c1.y == c4.y && c1.y == c5.y => false
    case _                                                                                         => true
  }
}
//def checkValid(c1: Coord, work: Work): Boolean = {
//  !(c1.right :: c1.right.right :: c1.right.right.right :: c1.right.right.right.right :: Nil).forall(c => work.visited.contains(c)) &&
//  !(c1.left :: c1.left.left :: c1.left.left.left:: c1.left.left.left.left :: Nil).forall(c => work.visited.contains(c)) &&
//  !(c1.north :: c1.north.north :: c1.north.north.north :: c1.north.north.north.north :: Nil).forall(c => work.visited.contains(c)) &&
//  !(c1.south :: c1.south.south :: c1.south.south.south :: c1.south.south.south.south :: Nil).forall(c => work.visited.contains(c))
//}

def resolveStar1(input: List[String]): Int = {
  val area = parse(input)
  val from = Coord(0, 0)
  val to   = Coord(area.maxX, area.maxY)

  val around = (work: Work) => {
    work.path.head.around
      .filterNot(c => c.x < 0 || c.y < 0 || c.x > area.maxX || c.y > area.maxY)
      .filter(c => checkValid(c, work))
  }

  val result = walk(
    from = from,
    around = around,
    goalReached = work => work.path.head == to,
    coord2weight = coord => area.cells(coord),
    dump(area)
  )
  result.head.weights.head
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle17Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        // puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 102
        // puzzleResult == 0
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
