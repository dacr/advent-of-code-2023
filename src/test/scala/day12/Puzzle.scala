package day12

import zio.*
import zio.test.*
import zio.test.TestAspect.*

case class Spring(pattern: String, damages: List[Int])

// ------------------------------------------------------------------------------
def parse(lines: List[String]) =
  lines.map(line =>
    line.split(" ", 2) match {
      case Array(pattern, rawDamages) =>
        Spring(pattern, rawDamages.trim.split(",").map(_.toInt).toList)
    }
  )

// ------------------------------------------------------------------------------

def arrangements(pattern: String, damages: List[Int]): Int = {
  val parts           = damages.head :: damages.map(count => " " + ("#" * count))
  val partsLength     = parts.map(_.size).sum
  val patternLength   = pattern.length
  val insertableCount = parts.size + 1
  val availableSpaces = patternLength - partsLength // how may space we can insert
  // TO be CONTINUED
}

def resolveStar1(input: List[String]): Int = {
  val springs = parse(input)
  springs
    .map(spring => arrangements(spring))
    .sum
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int =
  val springs = parse(input)
  0

// ------------------------------------------------------------------------------

object Puzzle12Test extends ZIOSpecDefault {
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
        exampleResult == 21,
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
