package day12

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec

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

def sumDecompose(sumGoal: Int): List[List[Int]] = {
  def decompose(goal: Int, accumulator: List[List[Int]]): List[List[Int]] = {
    if (goal == 0) accumulator
    else {
      val nextAccu =
        accumulator.flatMap(current =>
          val currentSum = current.sum
          if (currentSum == sumGoal) current :: Nil
          else 1.to(sumGoal - currentSum).map(n => n :: current)
        )
      decompose(goal - 1, nextAccu)
    }
  }
  decompose(sumGoal, 1.to(sumGoal).map(n => n :: Nil).toList)
}

def sumDecompose0(sumGoal: Int, length: Int): LazyList[List[Int]] = {
  def decompose(currentLength: Int, accumulator: LazyList[List[Int]]): LazyList[List[Int]] = {
    if (currentLength == 0) accumulator
    else {
      val nextAccu =
        accumulator.flatMap(current =>
          val currentSum = current.sum
          if (currentSum == sumGoal) (0 :: current) :: Nil
          else 0.to(sumGoal - currentSum).map(n => n :: current)
        )
      decompose(currentLength - 1, nextAccu)
    }
  }
  decompose(length - 1, LazyList.from(0.to(sumGoal).map(n => n :: Nil))).filter(_.sum == sumGoal)
}

def patternCheck(pattern: String, segments: List[String], configuration: List[Int]): Boolean = { // TODO not optimal !
  val patternFromConfig = segments.zip(configuration).map((s, c) => ("." * c) + s).mkString
  val result            = pattern.zip(patternFromConfig).forall((l, r) => l == r || l == '?')
  //println(s"$pattern\n$patternFromConfig $result\n")
  result
}

def arrangements(pattern: String, damages: List[Int]): Int = {
  println(s"$pattern $damages")
  val damagesSegments = damages.map(count => "#" * count)
  val segments        = (damagesSegments.head :: damagesSegments.tail.map(segment => "." + segment)) :+ ""
  val segmentsLength  = segments.map(_.size).sum
  val availableSpaces = pattern.length - segmentsLength // how may spaces we can insert between/arround
  val insertPlaces    = segments.size                   // how many places where we can insert spaces
  def configurations  = sumDecompose0(availableSpaces, insertPlaces)
  val result          = configurations.count(config => patternCheck(pattern, segments, config))
  println(s"   $result")
  result
}

def resolveStar1(input: List[String]): Int = {
  val springs = parse(input)
  springs
    .map(spring => arrangements(spring.pattern, spring.damages))
    .sum
}

// ------------------------------------------------------------------------------

def resolveStar2(input: List[String]): Int = {
  val springs = parse(input).map(from =>
    Spring(
      LazyList.continually(from.pattern).take(5).mkString("?"),
      LazyList.continually(from.damages).take(5).flatten.toList
    )
  )
  springs
    .map(spring => arrangements(spring.pattern, spring.damages))
    .sum
}

// ------------------------------------------------------------------------------

object Puzzle12Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("algorithms") {
      assertTrue(
        sumDecompose(1) == List(
          List(1)
        ),
        sumDecompose(2) == List(
          List(1, 1),
          List(2)
        ),
        sumDecompose(3) == List(
          List(1, 1, 1),
          List(2, 1),
          List(1, 2),
          List(3)
        ),
        sumDecompose(4) == List(
          List(1, 1, 1, 1),
          List(2, 1, 1),
          List(1, 2, 1),
          List(3, 1),
          List(1, 1, 2),
          List(2, 2),
          List(1, 3),
          List(4)
        ),
        sumDecompose0(3, 3) == List(
          List(3, 0, 0),
          List(2, 1, 0),
          List(1, 2, 0),
          List(0, 3, 0),
          List(2, 0, 1),
          List(1, 1, 1),
          List(0, 2, 1),
          List(1, 0, 2),
          List(0, 1, 2),
          List(0, 0, 3)
        )
      )
    },
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 21,
        puzzleResult == 8193
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 525152,
        puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
