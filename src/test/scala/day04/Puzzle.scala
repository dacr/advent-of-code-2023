package day04

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.util.chaining.*

case class ScratchCard(value: Int, winningNumbers: List[Int], numbers: List[Int]) {
  val winningNumbersSet  = winningNumbers.toSet
  val numbersSet         = numbers.toSet
  val winningNumberCount = numbersSet.intersect(winningNumbersSet).size
  val score              = winningNumberCount match {
    case 0 => 0
    case n => math.pow(2, n - 1).toInt
  }
}

// ------------------------------------------------------------------------------
val parseLineRE = """Card\s+(\d+)\s*:\s*([ 0-9]+)\s+\|\s+([ 0-9]+)""".r

def stringToInts(input: String) = input.trim.split("\\s+").map(_.toInt).toList

def parse(input: List[String]): List[ScratchCard] = {
  input.collect { case parseLineRE(pointsPart, winningPart, numbersPart) =>
    ScratchCard(
      value = pointsPart.toInt,
      winningNumbers = stringToInts(winningPart),
      numbers = stringToInts(numbersPart)
    )
  }
}

// ------------------------------------------------------------------------------

def resolveStar1(input: List[String]): Long =
  val cards = parse(input)
  cards
    .map(_.score)
    .sum

// ------------------------------------------------------------------------------
def resolveStar2(input: List[String]): Long = {
  val cards        = parse(input)
  val winsForCard  =
    cards.tails
      .filterNot(_.isEmpty)
      .map(cards => cards.head -> cards.drop(1).take(cards.headOption.map(_.winningNumberCount).getOrElse(0)))
      .toMap
  val initialState = cards.map(card => card -> 1).toMap
  val finalState   = cards.foldLeft(initialState) { case (state, card) =>
    val cardCount = state(card)
    val wonCards  = winsForCard(card)
    wonCards.foldLeft(state) { case (state, wonCard) =>
      state.updatedWith(wonCard)(valueOpt => valueOpt.map(_ + cardCount))
    }
  }
  finalState.values.sum
}

// ------------------------------------------------------------------------------

object Puzzle04Test extends ZIOSpecDefault {
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
        exampleResult == 13,
        puzzleResult == 25183
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 30,
        puzzleResult == 5667240
      )
    }
  ) @@ timed @@ sequential
}
