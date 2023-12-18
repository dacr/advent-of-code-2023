package day17

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math.*
import scala.io.AnsiColor.{RED_B, RESET, CYAN_B, BLUE_B, GREEN_B, BLUE, GREEN}

// ------------------------------------------------------------------------------

case class Coord(x: Int, y: Int) {
  def right  = Coord(x + 1, y)
  def left   = Coord(x - 1, y)
  def north  = Coord(x, y - 1)
  def south  = Coord(x, y + 1)
  def around = List(south, right, north, left)
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
case class Solution(path: Path, weight: Int)
case class Work(path: Path, visited: Set[Coord], weights: List[Int]) {
  def coord  = path.head
  def weight = weights.headOption.getOrElse(0)
}

def dump(area: Area)(work: Work, bestWeightAt: Map[Coord, Int]): Unit = {
  val pathCoords = work.path.toSet
  if (pathCoords.size != work.path.size) ???
  0.to(area.maxY).foreach { y =>
    0.to(area.maxX).foreach { x =>
      val coord = Coord(x, y)
      val value = area.cells(coord)
      if (pathCoords.contains(coord)) print(s"$RED_B$value$RESET")
      else if (bestWeightAt.contains(coord)) print(s"$BLUE$value$RESET")
      else print(value)
    }
    println()
  }
  // println(pathCoords.toList.init.map(c => area.cells(c)).sum)
  println(work.weights.mkString("-"))
}

def walk(
  from: Coord,
  to:Coord,
  around: Work => List[Coord],
  goalReached: Work => Boolean,
  coord2weight: Coord => Int,
  belowHint: Option[Int],
  dump: (Work, Map[Coord, Int]) => Unit
): List[Solution] = {
  @tailrec
  def worker(
    toVisit: Seq[Work],
    solutions: List[Solution],
    bestWeightAt: Map[Coord, Int],
    bestPathWeight: Option[Int],
    iter: Int
  ): List[Solution] = {
    if (iter % 3_000_000 == 0 && toVisit.size > 0) {
      dump(toVisit.head, bestWeightAt)
      println(s"${GREEN}solutions=${solutions.size} toVisit=${toVisit.size} bestPathWeight=$bestPathWeight$RESET")
    }
    toVisit match {
      case works if works.isEmpty =>
        solutions

      case Seq(work, remainWork*) if bestPathWeight.exists(best => work.weight > best) =>
        worker(remainWork, solutions, bestWeightAt, bestPathWeight, iter + 1)

      case works @ Seq(work, _*) if !bestWeightAt.contains(work.coord) =>
        worker(works, solutions, bestWeightAt + (work.coord -> work.weight), bestPathWeight, iter + 1)

      case Seq(work, remainWork*) if bestWeightAt.get(work.coord).exists(best => work.weight > best) =>
        worker(remainWork, solutions, bestWeightAt, bestPathWeight, iter + 1)

      case works @ Seq(work, _*) if bestWeightAt.get(work.coord).exists(best => work.weight < best) =>
        worker(works, solutions, bestWeightAt + (work.coord -> work.weight), bestPathWeight, iter + 1)

      case Seq(work, remainWork*) if goalReached(work) =>
        val newBestWeight = bestPathWeight.map(best => min(best, work.weight)).orElse(Some(work.weight))
        val newSolutions  = (Solution(work.path, work.weight) :: solutions).filterNot(_.weight > newBestWeight.get)
        //dump(work)
        worker(remainWork, newSolutions, bestWeightAt, newBestWeight, iter + 1)

      case Seq(work, remainWork*) =>
        val nextCoords  = around(work).filterNot(work.visited.contains)
        val nextToVisit = nextCoords.map { nextCoord =>
          Work(nextCoord :: work.path, work.visited + nextCoord, nextWeight(nextCoord, work, coord2weight))
        }.filterNot{work =>
          val toGoal =
              work.coord.x.to(to.x).foldLeft(0)((sum, x) => sum + coord2weight(Coord(x, work.coord.y))) +
                (work.coord.y).to(to.y).foldLeft(0)((sum,y) => coord2weight(Coord(work.coord.x, y)))
          //println(s"${work.weight + toGoal} - $belowHint")
          (work.weight + toGoal) > belowHint.get // TODO quick & dirty hack
        }
        worker(remainWork ++ nextToVisit, solutions, bestWeightAt, bestPathWeight, iter + 1) // BFS
        //worker(remainWork.prependedAll(nextToVisit), solutions, bestWeightAt, bestPathWeight, iter + 1) // DFS
    }
  }
  worker(Vector(Work(List(from), Set(from), Nil)), Nil, Map.empty, belowHint, 0)
}

def nextWeight(nextCoord: Coord, work: Work, coord2weight: Coord => Int): List[Int] = {
  work.path match {
    case Nil => Nil
    case _   => (work.weight + coord2weight(nextCoord)) :: work.weights
  }
}

// ------------------------------------------------------------------------------

def checkValid(c1: Coord, work: Work): Boolean = { // TODO not optimal !!
  work.path match {
    case c2 :: c3 :: c4 :: c5 :: _ if c1.x == c2.x && c1.x == c3.x && c1.x == c4.x && c1.x == c5.x => false
    case c2 :: c3 :: c4 :: c5 :: _ if c1.y == c2.y && c1.y == c3.y && c1.y == c4.y && c1.y == c5.y => false
    case _                                                                                         => true
  }
}

def resolveStar1(input: List[String]): Int = {
  val area = parse(input)
  val from = Coord(0, 0)
  val to   = Coord(area.maxX, area.maxY)

  val around = (work: Work) => {
    work.path.head.around
      .filterNot(c => c.x < 0 || c.y < 0 || c.x > area.maxX || c.y > area.maxY)
      .filter(c => checkValid(c, work))
  }

//  val belowHint =
//    from.x.to(to.x).map(x => area.cells(Coord(x, 0))).sum +
//      (from.y + 1).to(to.y).map(y => area.cells(Coord(to.x, y))).sum

//  val belowHint = to.x * 9 + to.y * 9
  //val belowHint = 987
  //val belowHint = 960
  val belowHint = 1200

  val result = walk(
    from = from,
    to = to,
    around = around,
    goalReached = work => work.path.head == to,
    coord2weight = coord => area.cells(coord),
    belowHint = Some(belowHint),
    dump(area)
  )
  println(s"result count = ${result.size} / first = ${result.head.weight}" )
  result.head.weight
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
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 102,
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
