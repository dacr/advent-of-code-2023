package day01

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
def parse(input: List[String]) =
  input

// ------------------------------------------------------------------------------

def filterDigits(input:String) = input.filter(_.isDigit)

def resolveStar1(input: List[String]): Int =
  input
    .map(filterDigits)
    .map(digits => ""+digits.head+digits.last)
    .map(_.toInt)
    .sum

// ------------------------------------------------------------------------------

val fixes = List(
//  "zero"->"0",
  "one"->"1",
  "two"->"2",
  "three"->"3",
  "four"->"4",
  "five"->"5",
  "six"->"6",
  "seven"->"7",
  "eight"->"8",
  "nine"->"9",
)

def findFirst(input:String):String = {
  fixes.find((key, num) => input.startsWith(key)) match {
    case Some((key, num)) => num
    case None if input.head.isDigit => input.head.toString
    case None => findFirst(input.tail)
  }
}

def findLast(input:String):String = {
  fixes.find((key, num) => input.endsWith(key)) match {
    case Some((key, num)) => num
    case None if input.last.isDigit => input.last.toString
    case None => findLast(input.init)
  }
}


def resolveStar2(input: List[String]): Int =
  input
    .map(input => "" + findFirst(input) + findLast(input))
    .map(_.toInt)
    .sum
// ------------------------------------------------------------------------------

object Puzzle01Test extends ZIOSpecDefault {
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
        exampleResult == 142,
        puzzleResult == 55172
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-2.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 281,
        puzzleResult != 54953,
        puzzleResult == 54925
      )
    }
  ) @@ timed @@ sequential
}
