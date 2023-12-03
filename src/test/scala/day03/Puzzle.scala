package day03

import zio.*
import zio.test.*
import zio.test.TestAspect.*

case class Coord(x: Int, y: Int) {
  def right  = Coord(x + 1, y)
  def left   = Coord(x - 1, y)
  def around = List(
    Coord(x + 1, y),
    Coord(x + 1, y - 1),
    Coord(x + 1, y + 1),
    Coord(x - 1, y),
    Coord(x - 1, y - 1),
    Coord(x - 1, y + 1),
    Coord(x, y + 1),
    Coord(x, y - 1)
  )
}

case class Schematic(content: Map[Coord, Char]) {
  val (digits, symbols) = content.keys.partition(coord => content.get(coord).exists(_.isDigit))

  val gears = symbols.filter(coord => content.get(coord).contains('*'))

  def numberAt(coord: Coord): Option[(Int, Set[Coord])] = {
    content.get(coord).map { case from =>
      val lefts  =
        LazyList
          .iterate(coord)(_.left)
          .takeWhile(current => content.get(current).exists(_.isDigit))
          .reverse
      val rights =
        LazyList
          .iterate(coord.right)(_.right)
          .takeWhile(current => content.get(current).exists(_.isDigit))
      val coords = lefts ++ rights
      val value  = coords.flatMap(current => content.get(current)).mkString.toInt
      value -> coords.toSet
    }
  }
}

// ------------------------------------------------------------------------------
def parse(input: List[String]): Schematic = {
  val cells = for {
    (line, y) <- input.zipWithIndex
    (cell, x) <- line.zipWithIndex
    if cell != '.'
  } yield Coord(x, y) -> cell
  Schematic(cells.toMap)
}

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Int = {
  val schematic = parse(input)
  schematic.symbols
    .flatMap(_.around)
    .flatMap(schematic.numberAt)
    .toList
    .distinctBy((value, coords) => coords)
    // .tapEach((value,coords)=> println(s"$value->$coords"))
    .map((value, coords) => value)
    .sum
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Long =
  val schematic = parse(input)
  schematic.gears
    .map(_.around.flatMap(schematic.numberAt).distinctBy((value, coords) => coords))
    .filter(_.size == 2)
    .map(found => found.map((value, coords) => value.toLong).reduce(_ * _))
    .sum

// ------------------------------------------------------------------------------

object Puzzle03Test extends ZIOSpecDefault {
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
        exampleResult == 4361,
        puzzleResult == 527144
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 467835,
        puzzleResult == 81463996
      )
    }
  ) @@ timed @@ sequential
}
