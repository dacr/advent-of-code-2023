package day02

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

enum Color {
  case red
  case green
  case blue
}

case class ColorPick(color: Color, count: Int)

case class Game(
  id: Int,
  revealed: List[List[ColorPick]]
)

// ------------------------------------------------------------------------------
def parsePicks(input: String): List[ColorPick] = {
  input
    .split("\\s*,\\s*")
    .map(part =>
      part.split("\\s+", 2) match {
        case Array(num, col) => ColorPick(Color.valueOf(col), num.toInt)
      }
    )
    .toList
}

def parseLine(input: String): Game = {
  input.split("\\s*:\\s*", 2) match {
    case Array(header, content) =>
      val id            = header.replaceFirst("Game ", "").toInt
      val revealedParts = content.split("\\s*;\\s*").toList
      val revealed      = revealedParts.map(parsePicks)
      Game(id, revealed)
  }
}

def parse(input: List[String]): List[Game] = {
  input.map(parseLine)
}

// ------------------------------------------------------------------------------

def gameMaxesFound(game: Game): Map[Color, Int] = {
  game.revealed.map(_.toList).flatten.groupMapReduce(_.color)(_.count)((a, b) => max(a, b))
}

def resolveStar1(input: List[String]): Int = {
  val games      = parse(input)
  val hypothesis = Map(Color.red -> 12, Color.green -> 13, Color.blue -> 14)
  val selected   = games.filter(game => gameMaxesFound(game).forall((color, count) => hypothesis(color) >= count))
  selected.map(_.id).sum
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val games = parse(input)
  games
    .map(game => gameMaxesFound(game).values.reduce(_ * _))
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle02Test extends ZIOSpecDefault {
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
        exampleResult == 8,
        puzzleResult == 2716
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 2286,
        puzzleResult == 72227
      )
    }
  ) @@ timed @@ sequential
}
