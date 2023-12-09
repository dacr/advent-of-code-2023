package day09

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
def parse(input: List[String]):List[List[Int]] =
  input.map(line => line.trim.split("\\s+").toList.map(_.toInt))

// ------------------------------------------------------------------------------

def nextValue(values:Iterable[Int]):Int = {
  if (values.forall(_ == 0 )) 0
  else {
    val differences = Iterable.from(values.sliding(2).map{ case a::b::Nil => b - a})
    val next = values.last + nextValue(differences)
    next
  }
}

def resolveStar1(input: List[String]): Int =
  val report = parse(input)
  report.map(nextValue).sum

// ------------------------------------------------------------------------------

def prevValue(values:Iterable[Int]):Int = {
  if (values.forall(_ == 0 )) 0
  else {
    val differences = Iterable.from(values.sliding(2).map{ case a::b::Nil => b - a})
    val prev = values.head - prevValue(differences)
    prev
  }
}

def resolveStar2(input: List[String]): Int =
  val report = parse(input)
  report.map(prevValue).sum

// ------------------------------------------------------------------------------

object Puzzle09Test extends ZIOSpecDefault {
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
        exampleResult == 114,
        puzzleResult == 1725987467
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 2,
        puzzleResult == 971
      )
    }
  ) @@ timed @@ sequential
}
