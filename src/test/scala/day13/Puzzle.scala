package day13

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.math.*

// ------------------------------------------------------------------------------
case class Pattern(rows: Vector[Vector[Char]], columns: Vector[Vector[Char]]) {
  override def toString: String = columns.map(_.mkString).mkString("\n")
}

def parse(input: String) =
  val rawPatterns = input.trim.split("\n\n").toList
  rawPatterns.map { rawPattern =>
    val rows    = rawPattern.trim.split("\n").toVector.map(_.toVector)
    val columns = rows.transpose
    Pattern(rows, columns)
  }

// ------------------------------------------------------------------------------

def mirrorIndex(input: Vector[Vector[Char]]): Option[Int] = {
  0.to(input.size - 2).find { index =>
    var leftIndex  = index
    var rightIndex = index + 1
    var mirrored   = true
    while (mirrored && leftIndex >= 0 && rightIndex < input.size) {
      mirrored = input(leftIndex) == input(rightIndex)
      leftIndex -= 1
      rightIndex += 1
    }
    mirrored
  }
}

def resolveStar1(input: String): Int = {
  val patterns    = parse(input)
  val horizontals = patterns.flatMap(pattern => mirrorIndex(pattern.rows).map(_ + 1))
  val verticals   = patterns.flatMap(pattern => mirrorIndex(pattern.columns).map(_ + 1))
  val notfound    = patterns.filter(pattern => mirrorIndex(pattern.rows).isEmpty && mirrorIndex(pattern.columns).isEmpty)
//  println(notfound.mkString("\n\n"))
  verticals.sum + horizontals.sum * 100
}

// ------------------------------------------------------------------------------

def resolveStar2(input: String): Int =
  0

// ------------------------------------------------------------------------------

object Puzzle13Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 405,
        puzzleResult > 31859,
        puzzleResult == 32371
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 0,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
